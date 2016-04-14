module Thermite.Login (spec, getState, deleteSession, module Web.Users.Remote.Types.Shared) where

import Prelude

import Data.Array (concat)
import Data.Bifunctor (lmap)
import Data.Either
import Data.Foldable (and)
import Data.Functor
import Data.JSON (class ToJSON, class FromJSON, parseJSON)
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
import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Trans

import Network.WebSockets.Sync.Socket as S
import Network.WebSockets.Sync.Request as R

import Web.Users.Remote.Types.Shared as RPC

import Thermite (get, modify)
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

import Unsafe.Coerce (unsafeCoerce)

import Thermite.Login.Model
import Thermite.Login.Model.Lenses
import Thermite.Login.Masks

import Debug.Trace

type Effects eff = (webStorage :: WebStorage.WebStorage, dom :: DOM.DOM, websocket :: S.WebSocket | eff)

render :: forall uid userdata err field eff. T.Render (State uid userdata err) (Config uid userdata err field (Effects eff)) (Action uid userdata err)
render dispatch props state _
  | state.redirectingAfterLogin = []
  | otherwise = case state.screen of
    LoginScreen -> container $ view T._render props.loginMask (dispatch <<< SubLoginAction) { locale: props.locale } state.loginState []
    RegisterScreen -> container $ view T._render props.registerMask (dispatch <<< SubRegisterAction) { locale: props.locale } state.regState []
    ResetPasswordScreen -> container $ view T._render props.resetPasswordMask (dispatch <<< SubResetAction) { locale: props.locale } state.resetPasswordState []
    SetNewPasswordScreen token -> container $ view T._render props.setNewPasswordMask (dispatch <<< SubSetNewPasswordAction) { locale: props.locale, token: token } state.setNewPasswordState []
    where
      container es = [ R.div [ RP.className "login-container" ] es ]

redirectToRoot :: forall eff. Eff (dom :: DOM.DOM, webStorage :: WebStorage.WebStorage | eff) Unit
redirectToRoot = do
  -- redirect to root
  window <- liftEff $ DOM.window
  location <- liftEff $ DOM.location window

  href <- WebStorage.getItem WebStorage.localStorage "href-before-login"
  WebStorage.removeItem WebStorage.localStorage "href-before-login"

  case href of
    Just href -> DOM.replace href location
    Nothing -> return unit

withSessionId :: forall uid userdata eff err. Boolean -> RPC.SessionId -> Aff (dom :: DOM.DOM, webStorage :: WebStorage.WebStorage | eff) (State uid userdata err -> State uid userdata err)
withSessionId redirect sessionId@(RPC.SessionId sid) = do
  liftEff $ do
    WebStorage.setItem WebStorage.localStorage "session" sid.unSessionId

    when redirect redirectToRoot

    WebStorage.removeItem WebStorage.localStorage "href-before-login"

    if redirect
      then return \st -> st { redirectingAfterLogin = true }
      else return \st -> st { sessionId = Just sessionId }

sendSync :: forall uid userdata err field b eff. (FromJSON b, ToJSON userdata, ToJSON uid, FromJSON uid, FromJSON err, ToJSON err) => (Config uid userdata err field (Effects eff)) -> (R.Proxy b -> UserCommand uid userdata err) -> Aff (Effects eff) (Either Error b)
sendSync cfg req = ((lmap error <$> parseJSON) =<<) <$> cfg.sendRequest (req R.Proxy)

performAction :: forall uid userdata err field eff. (ToJSON userdata, ToJSON uid, FromJSON uid, FromJSON err, ToJSON err)
              => T.PerformAction (Effects eff) (State uid userdata err) (Config uid userdata err field (Effects eff)) (Action uid userdata err)
performAction = handler
  where
    handler :: T.PerformAction (Effects eff) (State uid userdata err) (Config uid userdata err field (Effects eff)) (Action uid userdata err)

    handler (SubLoginAction Login) props state = when (not $ state.loginState.loginLoading) $ do
      modify $ set (loginState <<< loginLoading) true
      sessionId <- lift $ sendSync props $ RPC.AuthUser
        state.loginState.loginName
        state.loginState.loginPassword
        props.sessionLength

      case sessionId of
        Right (Just sessionId) -> lift (withSessionId false sessionId) >>= modify
        _ -> modify $ set (loginState <<< loginLoading) false
                  <<< set (loginState <<< loginError) true

    handler (SubLoginAction LoginWithFacebook) props state = lift $ do
      window <- liftEff $ DOM.window
      location <- liftEff $ DOM.location window
      origin <- liftEff $ DOM.origin location
      pathname <- liftEff $ DOM.pathname location
      href <- liftEff $ DOM.href location

      liftEff $ WebStorage.setItem WebStorage.localStorage "href-before-login" href

      facebookLoginUrl <- sendSync props (RPC.AuthFacebookUrl (encodeURIComponent $ origin ++ pathname ++ "#" ++ props.redirectUrl) [])

      case facebookLoginUrl of
        Right facebookLoginUrl -> liftEff $ DOM.replace facebookLoginUrl location
        Left _ -> return unit

    handler (SubLoginAction (LoginScreenChanged screen)) props state = do
      lift $ liftEff $ props.redirectToScreen screen

    handler (SubLoginAction cmd) props state = do
      view T._performAction (T.focusState loginState props.loginMask) cmd { locale: props.locale } state

    handler Logout props state = do
      lift $ liftEff $ WebStorage.removeItem WebStorage.localStorage "session"
      case state.sessionId of
        Just sessionId -> void $ lift $ sendSync props (RPC.Logout sessionId)
        Nothing -> return unit
      modify \_ -> emptyState props.defaultUserData

    handler (SubRegisterAction Register) props state = when (not $ state.regState.regLoading) $ do
      r <- lift $ sendSync props $ RPC.CreateUser
             state.regState.regEmail
             state.regState.regEmail
             state.regState.regPassword
             state.regState.regUserData
      case r of
        Right (Left err) -> modify $ set (regState <<< regResult) (Just $ Left err)
        Right (Right _) -> do
          modify $ set (regState <<< regResult) (Just $ Right unit)
          lift $ later' 1500 $ return unit -- delay for a bit before going back to login screen
          lift $ liftEff $ props.redirectToScreen LoginScreen
        Left  _ -> modify $ set (regState <<< regResult) Nothing

    handler (SubRegisterAction cmd) props state = do
      view T._performAction (T.focusState regState props.registerMask) cmd { locale: props.locale } state

    handler (SubResetAction ResetPassword) props state = when (not $ state.resetPasswordState.resetLoading) $ do
      modify $ set (resetPasswordState <<< resetLoading) true
      lift $ sendSync props (RPC.ResetPassword state.resetPasswordState.resetEmail)
      modify $ set (resetPasswordState <<< resetLoading) false
           <<< set (resetPasswordState <<< resetShowSuccessMessage) true
      lift $ later' 1500 $ return unit -- delay for a bit before going back to login screen
      lift $ liftEff $ props.redirectToScreen LoginScreen

    handler (SubResetAction cmd) props state = do
      view T._performAction (T.focusState resetPasswordState props.resetPasswordMask) cmd { locale: props.locale } state

    handler (SubSetNewPasswordAction (SetNewPassword token)) props state = when (not $ state.setNewPasswordState.setpwdLoading) $ do
      modify $ set (setNewPasswordState <<< setpwdLoading) true
      lift $ sendSync props (RPC.SetNewPassword token state.setNewPasswordState.setpwdPassword)
      modify $ set (setNewPasswordState <<< setpwdLoading) false
           <<< set (setNewPasswordState <<< setpwdShowSuccessMessage) true
      lift $ later' 1500 $ return unit -- delay for a bit before going back to login screen
      lift $ liftEff $ props.redirectToScreen LoginScreen

    handler (SubSetNewPasswordAction cmd) props state = do
      view T._performAction (T.focusState setNewPasswordState props.setNewPasswordMask) cmd { locale: props.locale, token: "" } state

    handler (ChangeScreen screen) props state = do
      modify $ \s -> (emptyState props.defaultUserData) { screen = screen, sessionId = s.sessionId }

spec :: forall uid userdata eff err field. (ToJSON userdata, FromJSON uid, ToJSON uid, FromJSON err, ToJSON err) => T.Spec (Effects eff) (State uid userdata err) (Config uid userdata err field (Effects eff)) (Action uid userdata err)
spec = T.simpleSpec performAction render

parseParams :: String -> Array (Tuple String String)
parseParams str = case stripPrefix "?" str of
  Just str -> map (toTuple <<< split "=") (split "&" str)
  Nothing -> []
    where toTuple [a, b] = Tuple a b
          toTuple _ = Tuple "" ""

deleteSession :: forall eff. Eff (Effects eff) Unit
deleteSession = WebStorage.removeItem WebStorage.localStorage "session"

getState :: forall uid userdata eff err field. (ToJSON userdata, ToJSON uid, FromJSON uid, ToJSON err, FromJSON err) => Config uid userdata err field (Effects eff) -> Aff (Effects eff) (Maybe (State uid userdata err))
getState props = do
  window <- liftEff $ DOM.window
  location <- liftEff $ DOM.location window
  origin <- liftEff $ DOM.origin location
  pathname <- liftEff $ DOM.pathname location
  hash <- liftEff $ DOM.hash location
  search <- liftEff $ DOM.search location

  if hash == "#" ++ props.redirectUrl
    then do
      token <- sendSync props (RPC.AuthFacebook (origin ++ pathname ++ "#" ++ props.redirectUrl) (parseParams search) props.defaultUserData props.sessionLength)

      case token of
        Left _ -> liftEff $ do
          deleteSession
          redirectToRoot
          return Nothing
        Right (Left _) -> liftEff $ do
          deleteSession
          redirectToRoot
          return Nothing
        Right (Right sessionId) -> do
          with <- withSessionId true sessionId
          return $ Just (with $ emptyState props.defaultUserData)
    else do
      session <- liftEff $ WebStorage.getItem WebStorage.localStorage "session"
      case session of
        Just session -> return $ Just $ (emptyState props.defaultUserData) { sessionId = Just $ RPC.SessionId { unSessionId: session } }
        Nothing -> return Nothing
