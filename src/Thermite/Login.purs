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
    LoginScreen -> container renderLoginScreen
    RegisterScreen -> container renderRegisterScreen
    ResetPasswordScreen -> container renderResetPasswordScreen
    SetNewPasswordScreen token -> container (renderSetNewPasswordScreen token)

    where
      container es = [ R.div [ RP.className "login-container" ] es ]
      renderLoginScreen = concat
        [ textinput props.locale.name (loginState <<< loginName)
        , textinput' true validateAlways (if state.loginState.loginLoading then Nothing else Just Login) props.locale.password (loginState <<< loginPassword)
        , [ button
              true
              state.loginState.loginLoading
              props.locale.login
              "login-button-login"
              dispatch
              Login
          , R.div
            [ RP.className "login-text-container" ]
            [ R.div
              [ RP.onClick \_ -> dispatch (ScreenChanged ResetPasswordScreen)
              , RP.className "login-text-forgot-password"
              ]
              [ R.text props.locale.forgotPassword ]
            , R.div
              [ RP.onClick \_ -> dispatch (ScreenChanged RegisterScreen)
              , RP.className "login-text-register"
              ]
              [ R.text props.locale.register ]
            ]
          , R.div [ RP.className "login-divider" ] []
          , R.div
            [ RP.onClick \_ -> dispatch LoginWithFacebook
            , RP.className "login-button-facebook"
            ]
            [ R.text props.locale.loginWithFacebook ]
          ]
        , if state.loginState.loginError
             then [ R.div
                    [ RP.className "login-error" ]
                    [ R.text props.locale.errUserOrPasswordIncorrect ]
                  ]
             else [ ]
        ]

      renderRegisterScreen = concat
        [ textinput props.locale.name (regState <<< regName)
        , textinput props.locale.email (regState <<< regEmail)
        , concat $ flip map props.additionalFields \af ->
            textinput (props.locale.additionalFieldTitle af.field)
                      (regState <<< regUserData <<< uncurry lens af.fieldLens)
        , textinput' true validateAlways Nothing props.locale.password (regState <<< regPassword)
        , textinput' true (hoistValidator regState $ validateRepeatPassword regPassword regRepeatPassword) (Just Register) props.locale.repeatPassword (regState <<< regRepeatPassword)
        , [ button
              ( allValid state
                $ [ hoistValidator regState validateEmail
                  , hoistValidator regState $ validateRepeatPassword regPassword regRepeatPassword
                  ]
                  ++ map (\f -> hoistValidator (regState <<< regUserData) f.validate)
                         props.additionalFields
              )
              state.regState.regLoading
              props.locale.register
              "login-button-register"
              dispatch
              Register
          ]
        , case state.regState.regResult of
            Just (Left (RPC.CreateUserValidationError err)) -> [ R.div [ RP.className "login-register-error" ] [ R.text $ props.locale.userDataValidationError err ] ]
            Just (Left _) -> [ R.div [ RP.className "login-register-error" ] [ R.text props.locale.errUserOrEmailAlreadyTaken ] ]
            Just (Right _) ->  [ R.div [ RP.className "login-register-success" ] [ R.text props.locale.userCreatedSuccessfully ] ]
            _ ->  []
        ]

      renderResetPasswordScreen = concat
        [ textinput' false validateAlways (Just ResetPassword) props.locale.email (resetPasswordState <<< resetEmail)
        , [ button
              true
              state.resetPasswordState.resetLoading
              props.locale.resetPassword
              "login-button-reset-password"
              dispatch
              ResetPassword
          ]
        , if state.resetPasswordState.resetShowSuccessMessage
            then [ R.div [ RP.className "login-register-success" ] [ R.text $ props.locale.passwordResetMailSentSuccessfully ] ]
            else []
        ]

      renderSetNewPasswordScreen token = concat
        [ textinput' true validateAlways Nothing props.locale.password (setNewPasswordState <<< setpwdPassword)
        , textinput' true (hoistValidator setNewPasswordState $ validateRepeatPassword setpwdPassword setpwdRepeatPassword) (Just $ SetNewPassword token) props.locale.repeatPassword (setNewPasswordState <<< setpwdRepeatPassword)
        , [ button
              (validateRepeatPassword setpwdPassword setpwdRepeatPassword state.setNewPasswordState)
              state.setNewPasswordState.setpwdLoading
              props.locale.setPassword
              "login-button-register"
              dispatch
              (SetNewPassword token)
          ]
        , if state.setNewPasswordState.setpwdShowSuccessMessage
            then [ R.div [ RP.className "login-register-success" ] [ R.text $ props.locale.newPasswordSetSuccessfully ] ]
            else []
        ]

      textinput' :: Boolean
                 -> Validator (State uid userdata err)
                 -> Maybe (Action uid userdata err)
                 -> String
                 -> Lens (State uid userdata err) (State uid userdata err) String String
                 -> _
      textinput' pwd validate onEnter text lens =
        [ R.input
          [ RP.onChange \e -> dispatch $ TextChanged lens ((unsafeCoerce e).target.value)
          , RP.onKeyDown \e -> case onEnter of
              Just action | e.keyCode == 13 -> dispatch action
              _ -> return unit
          , RP.value (state ^. lens)
          , RP.placeholder text
          , if validate state
               then RP.className "login-input"
               else RP.className "login-input login-input-error"
          , if pwd then RP._type "password" else RP._type ""
          ] []
        ]

      textinput = textinput' false validateAlways Nothing

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
    handler Login props state = when (not $ state.loginState.loginLoading) $ do
      modify $ set (loginState <<< loginLoading) true
      sessionId <- lift $ sendSync props $ RPC.AuthUser
        state.loginState.loginName
        state.loginState.loginPassword
        props.sessionLength

      case sessionId of
        Right (Just sessionId) -> lift (withSessionId false sessionId) >>= modify
        _ -> modify $ set (loginState <<< loginLoading) false
                  <<< set (loginState <<< loginError) true

    handler LoginWithFacebook props state = lift $ do
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

    handler Logout props state = do
      lift $ liftEff $ WebStorage.removeItem WebStorage.localStorage "session"
      case state.sessionId of
        Just sessionId -> void $ lift $ sendSync props (RPC.Logout sessionId)
        Nothing -> return unit
      modify \_ -> emptyState props.defaultUserData

    handler Register props state = when (not $ state.regState.regLoading) $ do
      r <- lift $ sendSync props $ RPC.CreateUser
             state.regState.regName
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
    handler ResetPassword props state = when (not $ state.resetPasswordState.resetLoading) $ do
      modify $ set (resetPasswordState <<< resetLoading) true
      lift $ sendSync props (RPC.ResetPassword state.resetPasswordState.resetEmail)
      modify $ set (resetPasswordState <<< resetLoading) false
           <<< set (resetPasswordState <<< resetShowSuccessMessage) true
      lift $ later' 1500 $ return unit -- delay for a bit before going back to login screen
      lift $ liftEff $ props.redirectToScreen LoginScreen
    handler (SetNewPassword token) props state = when (not $ state.setNewPasswordState.setpwdLoading) $ do
      modify $ set (setNewPasswordState <<< setpwdLoading) true
      lift $ sendSync props (RPC.SetNewPassword token state.setNewPasswordState.setpwdPassword)
      modify $ set (setNewPasswordState <<< setpwdLoading) false
           <<< set (setNewPasswordState <<< setpwdShowSuccessMessage) true
      lift $ later' 1500 $ return unit -- delay for a bit before going back to login screen
      lift $ liftEff $ props.redirectToScreen LoginScreen
    handler (TextChanged lens v) _ state = do
      modify $ set lens v
    handler (ChangeScreen screen) props state = do
      modify $ \s -> (emptyState props.defaultUserData) { screen = screen, sessionId = s.sessionId }
    handler (ScreenChanged screen) props state = do
      lift $ liftEff $ props.redirectToScreen screen

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
