module Main where

import Prelude

import Data.Array (concat)
import Data.Either
import Data.Functor
import Data.JSON
import Data.Lens
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)

import Control.Alt
import Control.Apply
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Console.Unsafe
import Control.Monad.Eff.Exception (throwException, EXCEPTION())

import Network.WebSockets.Sync.Socket as S
import Network.WebSockets.Sync.Request as R

import Web.Users.Remote.Types.Shared as RPC

import Thermite as T
import Thermite.Aff as T

import DOM as DOM
import DOM.HTML as DOM
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

import Model
import Model.Lenses

type UserCommand = RPC.UserCommand String Int String

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
          [ RP.onClick \_ -> dispatch Login
          , RP.href $ state.facebookUrl
          ]
          [ R.text "Login with Facebook" ]
        , R.div
          [ RP.onClick \_ -> dispatch (ChangeScreen RegisterScreen) ]
          [ R.text "Register" ]
        ]
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

performAction :: T.PerformAction _ State _ Action
performAction = T.asyncOne' handler
  where
    handler :: Action -> _ -> State -> Aff _ (State -> State)
    handler Login _ state = do
      url <- sendSync state.socket (RPC.AuthFacebookUrl "" [])
      pure id
    handler Register _ state = do
      r <- sendSync state.socket $ RPC.CreateUser (RPC.User
        { u_name: state.regState.regName
        , u_email: state.regState.regEmail
        , u_password: RPC.PasswordHidden
        , u_active: true
        , u_more: "none" }) state.regState.regPassword
      pure $ case r of
        Left  _ -> set (regState <<< regResult) (Just $ Left unit)
        Right _ -> set (regState <<< regResult) (Just $ Right unit)
    handler (TextChanged lens v) _ state = do
      pure $ \s -> set lens v s
    handler (ChangeScreen screen) _ state = do
      case screen of
        LoginScreen -> setHash "login"
        RegisterScreen -> setHash "register"
        ResetPasswordScreen -> setHash "reset-password"
      pure $ \s -> s { screen = screen }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

route :: Match Screen
route = LoginScreen <$ (lit "login")
    <|> RegisterScreen <$ (lit "register")
    <|> ResetPasswordScreen <$ (lit "reset-password")

main :: forall e. Eff (dom :: DOM.DOM, websocket :: S.WebSocket, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  socket <- S.connect "ws://localhost:8538" true
    { connected : \_ -> return unit
    , disconnected : return unit
    , message : \_ -> return unit
    }

  match <- matchHash route <$> getHash

  case match of
    Right screen -> do
      runAff throwException (const (pure unit)) $ do
        url <- sendSync socket (RPC.AuthFacebookUrl "" [])

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
