module Thermite.Login where

import Prelude

import Data.Array (concat)
import Data.Either
import Data.Functor
import Data.JSON
import Data.Lens
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import Data.String
import Data.Tuple

import Browser.WebStorage as WebStorage
import Global

import Control.Alt
import Control.Apply
import Control.Coroutine (emit)
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Console.Unsafe
import Control.Monad.Eff.Exception (throwException, EXCEPTION())
import Control.Monad.Trans

import Network.WebSockets.Sync.Socket as S
import Network.WebSockets.Sync.Request as R

import Web.Users.Remote.Types.Shared as RPC

import Thermite as T

import DOM as DOM
import DOM.HTML as DOM
import DOM.HTML.Location as DOM
import DOM.HTML.Types as DOM
import DOM.HTML.Window as DOM
import DOM.Node.ParentNode as DOM

import React as R
import React.DOM as R
import React.DOM.Props as RP

import Routing
import Routing.Hash (getHash)
import Routing.Hash.Aff (setHash)
import Routing.Match
import Routing.Match.Class

import Unsafe.Coerce

import Thermite.Login.Model
import Thermite.Login.Model.Lenses

type UserCommand = RPC.UserCommand String Int RPC.SessionId

-- specialized sendSync
sendSync :: forall eff b. (FromJSON b) => S.Socket -> (R.Proxy b -> UserCommand) -> Aff (websocket :: S.WebSocket | eff) b
sendSync = R.sendSync

render :: T.Render State _ Action
render dispatch _ state _ = case state.screen of
  LoginScreen -> renderLoginScreen
  RegisterScreen -> renderRegisterScreen
  ResetPasswordScreen -> renderResetPasswordScreen

  where
    renderLoginScreen = concat
      [ textinput "Name" (loginState <<< loginName)
      , textinput "Password" (loginState <<< loginPassword)
      , [ R.div
          [ RP.onClick \_ -> dispatch Login ]
          [ R.text "Login" ]
        , R.div
          [ RP.onClick \_ -> dispatch (ChangeScreen ResetPasswordScreen) ]
          [ R.text "Forgot password" ]
        , R.a
          [ RP.href $ state.facebookUrl
          ]
          [ R.text "Login with Facebook" ]
        , R.div
          [ RP.onClick \_ -> dispatch (ChangeScreen RegisterScreen) ]
          [ R.text "Register" ]
        ]
      , case state.loginState.loginSession of
          Just (Left _) -> [ R.div [] [ R.text "Error" ] ]
          Just (Right _) ->  [ R.div [] [ R.text "Logged in successfully" ] ]
          _ ->  []
      ]

    renderRegisterScreen = concat
      [ textinput "Name" (regState <<< regName)
      , textinput "Email" (regState <<< regEmail)
      , textinput "Password" (regState <<< regPassword)
      , textinput "Repeat password" (regState <<< regRepeatPassword)
      , [ R.div
          [ RP.onClick \_ -> dispatch Register ]
          [ R.text "Register" ]
        ]
      , case state.regState.regResult of
          Just (Left _) -> [ R.div [] [ R.text "Error" ] ]
          Just (Right _) ->  [ R.div [] [ R.text "User created succesfully" ] ]
          _ ->  []
      ]

    renderResetPasswordScreen = concat
      [ textinput "Email" (resetPasswordState <<< resetEmail)
      , [ R.div
          [ RP.onClick \_ -> dispatch Register ]
          [ R.text "Reset password" ]
        ]
      ]

    textinput :: String -> Lens State State String String -> _
    textinput text lens =
      [ R.div [] [ R.text text ]
      , R.input
        [ RP.onChange \e -> dispatch $ TextChanged lens ((unsafeCoerce e).target.value)
        , RP.value (state ^. lens)
        ] []
      ]

-- TODO: config this
sessionLength :: Int
sessionLength = 3600 * 60

performAction :: T.PerformAction _ State _ Action
performAction = handler
  where
    handler :: T.PerformAction _ State _ Action
    handler Login _ state = do
      r <- lift $ sendSync state.socket $ RPC.AuthUser
        state.loginState.loginName
        state.loginState.loginPassword
        sessionLength
      emit $ case r of
        Just sid -> set (loginState <<< loginSession) (Just $ Right sid)
        Nothing  -> set (loginState <<< loginSession) (Just $ Left unit)
    handler Register _ state = do
      r <- lift $ sendSync state.socket $ RPC.CreateUser (RPC.User
        { u_name: state.regState.regName
        , u_email: state.regState.regEmail
        , u_password: RPC.PasswordHidden
        , u_active: true
        , u_more: "none" }) state.regState.regPassword
      emit $ case r of
        Left  _ -> set (regState <<< regResult) (Just $ Left unit)
        Right _ -> set (regState <<< regResult) (Just $ Right unit)
    handler ResetPassword _ state = do
      return unit
    handler (TextChanged lens v) _ state = do
      emit $ set lens v
    handler (ChangeScreen screen) _ state = do
      case screen of
        LoginScreen -> lift $ setHash "login"
        RegisterScreen -> lift $ setHash "register"
        ResetPasswordScreen -> lift $ setHash "reset-password"
      emit $ \s -> s { screen = screen }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

route :: Match Screen
route = LoginScreen <$ (lit "")
    <|> LoginScreen <$ (lit "login")
    <|> RegisterScreen <$ (lit "register")
    <|> ResetPasswordScreen <$ (lit "reset-password")

parseParams :: String -> Array (Tuple String String)
parseParams str = case stripPrefix "?" str of
  Just str -> map (toTuple <<< split "=") (split "&" str)
  Nothing -> []
    where toTuple [a, b] = Tuple a b
          toTuple _ = Tuple "" ""

-- NOTE: main should take (SessionId -> Aff (Maybe Spec)) and yield (Aff Spec)
main :: forall e. Eff (webStorage :: WebStorage.WebStorage, dom :: DOM.DOM, websocket :: S.WebSocket, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  -- session <- WebStorage.getItem WebStorage.localStorage "session"

  socket <- S.connect "ws://localhost:8538" true
    { connected : \_ -> return unit
    , disconnected : return unit
    , message : \_ -> return unit
    }

  match <- matchHash route <$> getHash
  window <- DOM.window
  location <- DOM.location window
  origin <- DOM.origin location
  pathname <- DOM.pathname location
  hash <- DOM.hash location
  search <- DOM.search location

  runAff throwException (const (pure unit)) $ do
    if hash == "#oauth-login"
      then do
        token <- sendSync socket (RPC.AuthFacebook (origin ++ pathname ++ "#oauth-login") (parseParams search) sessionLength)
        liftEff $ logAny token
      else case match of
        Right screen -> do
          url <- sendSync socket (RPC.AuthFacebookUrl (encodeURIComponent $ origin ++ pathname ++ "#oauth-login") [])

          liftEff $ do
            let changeHash this _ screen = do
                  R.transformState this \s -> s { screen = screen }
                  return unit

                reactSpec = T.createReactSpec spec (emptyState socket url)
                component = R.createClass (reactSpec.spec { componentWillMount = \this -> matches route (changeHash this) })
                factory = R.createFactory component {}

            document <- DOM.window >>= DOM.document
            container <- fromJust <<< toMaybe <$> DOM.querySelector "#main" (DOM.htmlDocumentToParentNode document)

            this <- R.render factory container
            return unit
        _ -> return unit
