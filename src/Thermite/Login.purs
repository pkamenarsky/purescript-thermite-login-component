module Thermite.Login (spec, getState, module Web.Users.Remote.Types.Shared) where

import Prelude

import Data.Array (concat)
import Data.Either
import Data.Functor
import Data.JSON
import Data.Lens
import Data.Maybe
import Data.Nullable (toMaybe)
import Data.String
import Data.Tuple

import Browser.WebStorage as WebStorage
import Global

import Control.Alt
import Control.Apply
import Control.Bind
import Control.Coroutine (emit)
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
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

import Unsafe.Coerce

import Thermite.Login.Model
import Thermite.Login.Model.Lenses

type UserCommand userdata = RPC.UserCommand userdata Int RPC.SessionId

type Effects eff = (webStorage :: WebStorage.WebStorage, dom :: DOM.DOM, websocket :: S.WebSocket | eff)

render :: forall uid userdata. T.Render (State uid userdata) (Config userdata) (Action uid userdata)
render dispatch props state _
  | state.redirectingAfterLogin = []
  | otherwise = case state.screen of
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
          , R.div
            [ RP.onClick \_ -> dispatch LoginWithFacebook
            ]
            [ R.text "Login with Facebook" ]
          , R.div
            [ RP.onClick \_ -> dispatch (ChangeScreen RegisterScreen) ]
            [ R.text "Register" ]
          ]
        , case state.sessionId of
            Nothing -> [ R.div [] [ R.text "Error" ] ]
            Just _  -> [ R.div [] [ R.text "Logged in successfully" ] ]
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

      textinput :: String -> Lens (State uid userdata) (State uid userdata) String String -> _
      textinput text lens =
        [ R.div [] [ R.text text ]
        , R.input
          [ RP.onChange \e -> dispatch $ TextChanged lens ((unsafeCoerce e).target.value)
          , RP.value (state ^. lens)
          ] []
        ]

withSessionId :: forall uid userdata eff. Boolean -> RPC.SessionId -> Aff (dom :: DOM.DOM, webStorage :: WebStorage.WebStorage | eff) (State uid userdata -> State uid userdata)
withSessionId redirect sessionId@(RPC.SessionId sid) = do
  liftEff $ do
    WebStorage.setItem WebStorage.localStorage "session" sid.unSessionId

    -- redirect to root
    window <- liftEff $ DOM.window
    location <- liftEff $ DOM.location window

    href <- WebStorage.getItem WebStorage.localStorage "href-before-login"
    WebStorage.removeItem WebStorage.localStorage "href-before-login"

    case href of
      Just href -> if redirect then DOM.replace href location else return unit
      Nothing -> return unit

    if redirect
      then return \st -> st { redirectingAfterLogin = true }
      else return \st -> st { sessionId = Just sessionId }

performAction :: forall uid userdata eff. (ToJSON userdata)
              => T.PerformAction (Effects eff) (State uid userdata) (Config userdata) (Action uid userdata)
performAction = handler
  where
    -- specialized sendSync
    sendSync :: forall b. (FromJSON b) => S.Socket -> (R.Proxy b -> UserCommand userdata) -> Aff (Effects eff) b
    sendSync = R.sendSync

    handler :: T.PerformAction (Effects eff) (State uid userdata) (Config userdata) (Action uid userdata)
    handler Login props state = do
      sessionId <- lift $ sendSync props.socket $ RPC.AuthUser
        state.loginState.loginName
        state.loginState.loginPassword
        props.sessionLength

      case sessionId of
        Just sessionId -> lift (withSessionId false sessionId) >>= emit
        Nothing -> return unit
    handler LoginWithFacebook props state = lift $ do
      window <- liftEff $ DOM.window
      location <- liftEff $ DOM.location window
      origin <- liftEff $ DOM.origin location
      pathname <- liftEff $ DOM.pathname location
      href <- liftEff $ DOM.href location

      liftEff $ WebStorage.setItem WebStorage.localStorage "href-before-login" href

      facebookLoginUrl <- sendSync props.socket (RPC.AuthFacebookUrl (encodeURIComponent $ origin ++ pathname ++ "#" ++ props.redirectUrl) [])

      liftEff $ DOM.replace facebookLoginUrl location

    handler Register props state = do
      r <- lift $ sendSync props.socket $ RPC.CreateUser (RPC.User
        { u_name: state.regState.regName
        , u_email: state.regState.regEmail
        , u_password: RPC.PasswordHidden
        , u_active: true
        , u_more: props.defaultUserData }) state.regState.regPassword
      emit $ case r of
        Left  _ -> set (regState <<< regResult) (Just $ Left unit)
        Right _ -> set (regState <<< regResult) (Just $ Right unit)
    handler ResetPassword _ state = do
      return unit
    handler (TextChanged lens v) _ state = do
      emit $ set lens v
    handler (ChangeScreen screen) _ state = do
      emit $ \s -> s { screen = screen }

spec :: forall uid userdata eff. (ToJSON userdata) => T.Spec (Effects eff) (State uid userdata) (Config userdata) (Action uid userdata)
spec = T.simpleSpec performAction render

parseParams :: String -> Array (Tuple String String)
parseParams str = case stripPrefix "?" str of
  Just str -> map (toTuple <<< split "=") (split "&" str)
  Nothing -> []
    where toTuple [a, b] = Tuple a b
          toTuple _ = Tuple "" ""

getState :: forall uid userdata eff. (ToJSON userdata) => Config userdata -> Aff (Effects eff) (Maybe (State uid userdata))
getState props = do
  window <- liftEff $ DOM.window
  location <- liftEff $ DOM.location window
  origin <- liftEff $ DOM.origin location
  pathname <- liftEff $ DOM.pathname location
  hash <- liftEff $ DOM.hash location
  search <- liftEff $ DOM.search location

  let -- specialized sendSync
      sendSync :: forall b. (FromJSON b) => S.Socket -> (R.Proxy b -> UserCommand userdata) -> Aff (Effects eff) b
      sendSync = R.sendSync

  if hash == "#" ++ props.redirectUrl
    then do
      token <- sendSync props.socket (RPC.AuthFacebook (origin ++ pathname ++ "#" ++ props.redirectUrl) (parseParams search) props.sessionLength)

      case token of
        Left _ -> return Nothing
        Right sessionId -> do
          with <- withSessionId true sessionId
          return $ Just (with emptyState)
    else do
      session <- liftEff $ WebStorage.getItem WebStorage.localStorage "session"
      case session of
        Just session -> return $ Just $ emptyState { sessionId = Just $ RPC.SessionId { unSessionId: session } }
        Nothing -> return Nothing
