module Thermite.Login (spec, getState, deleteSession, module Web.Users.Remote.Types.Shared) where

import Prelude

import Data.Array (concat)
import Data.Either
import Data.Foldable (and)
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
import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error)
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

import Unsafe.Coerce

import Thermite.Login.Model
import Thermite.Login.Model.Lenses

import Debug.Trace

type UserCommand userdata = RPC.UserCommand userdata Int RPC.SessionId

type Effects eff = (webStorage :: WebStorage.WebStorage, dom :: DOM.DOM, websocket :: S.WebSocket | eff)

type Validator a b err = State a b err -> Boolean

allValid :: forall a b err. State a b err -> Array (Validator a b err) -> Boolean
allValid st = and <<< map ($ st)

validateAlways :: forall a b err. Validator a b err
validateAlways _ = true

validateFullName :: forall a b err. Validator a b err
validateFullName st = not null st.regState.regFullName

validateEmail :: forall a b err. Validator a b err
validateEmail st = not null st.regState.regEmail

validateRepeatPassword :: forall a b err. Validator a b err
validateRepeatPassword st = not null st.regState.regPassword
                         && st.regState.regPassword == st.regState.regRepeatPassword

button :: forall uid userdata err. Boolean -> Boolean -> String -> String -> (Action uid userdata err -> T.EventHandler) -> Action uid userdata err -> R.ReactElement
button valid loading text className dispatch action =
  R.div
    (case Tuple valid loading of
      Tuple true true -> [ RP.className className ]
      Tuple true false ->
        [ RP.onClick \_ -> dispatch action
        , RP.className className
        ]
      Tuple false _ -> [ RP.className $ className ++ " " ++ "login-button-disabled" ]
    )
    [ if loading
        then R.div [ RP.className "login-button-loading icon ion-ios-loop-strong" ] []
        else R.text text
    ]

render :: forall uid userdata err. T.Render (State uid userdata err) (Config userdata err) (Action uid userdata err)
render dispatch props state _
  | state.redirectingAfterLogin = []
  | otherwise = case state.screen of
    LoginScreen -> container renderLoginScreen
    RegisterScreen -> container renderRegisterScreen
    ResetPasswordScreen -> container renderResetPasswordScreen

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
        , textinput props.locale.fullName (regState <<< regFullName)
        , textinput props.locale.email (regState <<< regEmail)
        , textinput' true validateAlways Nothing props.locale.password (regState <<< regPassword)
        , textinput' true validateRepeatPassword (Just Register) props.locale.repeatPassword (regState <<< regRepeatPassword)
        , [ button
              (allValid state [validateFullName, validateEmail, validateRepeatPassword])
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
        ]

      textinput' :: Boolean
                 -> (State uid userdata err -> Boolean)
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

withSessionId :: forall uid userdata eff err. Boolean -> RPC.SessionId -> Aff (dom :: DOM.DOM, webStorage :: WebStorage.WebStorage | eff) (State uid userdata err -> State uid userdata err)
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

performAction :: forall uid userdata err eff. (ToJSON userdata, FromJSON err, ToJSON err)
              => T.PerformAction (Effects eff) (State uid userdata err) (Config userdata err) (Action uid userdata err)
performAction = handler
  where
    -- specialized sendSync
    sendSync :: forall b. (FromJSON b) => S.Socket -> (R.Proxy b -> UserCommand userdata err) -> Aff (Effects eff) (Either Error b)
    sendSync = R.sendSync

    handler :: T.PerformAction (Effects eff) (State uid userdata err) (Config userdata err) (Action uid userdata err)
    handler Login props state = when (not $ state.loginState.loginLoading) $ do
      modify $ set (loginState <<< loginLoading) true
      sessionId <- lift $ sendSync props.socket $ RPC.AuthUser
        state.loginState.loginName
        state.loginState.loginPassword
        props.sessionLength

      case sessionId of
        Right (Just sessionId) -> lift (withSessionId false sessionId) >>= modify
        Left _ -> modify $ set (loginState <<< loginLoading) false
                       <<< set (loginState <<< loginError) true

    handler LoginWithFacebook props state = lift $ do
      window <- liftEff $ DOM.window
      location <- liftEff $ DOM.location window
      origin <- liftEff $ DOM.origin location
      pathname <- liftEff $ DOM.pathname location
      href <- liftEff $ DOM.href location

      liftEff $ WebStorage.setItem WebStorage.localStorage "href-before-login" href

      facebookLoginUrl <- sendSync props.socket (RPC.AuthFacebookUrl (encodeURIComponent $ origin ++ pathname ++ "#" ++ props.redirectUrl) [])

      case facebookLoginUrl of
        Right facebookLoginUrl -> liftEff $ DOM.replace facebookLoginUrl location
        Left _ -> return unit

    handler Logout props state = do
      lift $ liftEff $ WebStorage.removeItem WebStorage.localStorage "session"
      case state.sessionId of
        Just sessionId -> do
          void $ lift $ sendSync props.socket (RPC.Logout sessionId)
        Nothing -> return unit
      modify \_ -> emptyState

    handler Register props state = when (not $ state.regState.regLoading) $ do
      r <- lift $ sendSync props.socket $ RPC.CreateUser
             state.regState.regName
             state.regState.regEmail
             state.regState.regPassword
      case r of
        Right (Left err) -> modify $ set (regState <<< regResult) (Just $ Left err)
        Right (Right _) -> do
          modify $ set (regState <<< regResult) (Just $ Right unit)
          -- lift $ later' 1500 $ return unit -- delay for a bit before going back to login screen
          -- modify $ set screen LoginScreen
        Left  _ -> modify $ set (regState <<< regResult) Nothing
    handler ResetPassword _ state = when (not $ state.resetPasswordState.resetLoading) $ do
      modify $ set screen LoginScreen
    handler (TextChanged lens v) _ state = do
      modify $ set lens v
    handler (ChangeScreen screen) _ state = do
      modify $ \s -> s { screen = screen }
    handler (ScreenChanged screen) _ state = do
      modify $ \s -> s { screen = screen }

spec :: forall uid userdata eff err. (ToJSON userdata, FromJSON err, ToJSON err) => T.Spec (Effects eff) (State uid userdata err) (Config userdata err) (Action uid userdata err)
spec = T.simpleSpec performAction render

parseParams :: String -> Array (Tuple String String)
parseParams str = case stripPrefix "?" str of
  Just str -> map (toTuple <<< split "=") (split "&" str)
  Nothing -> []
    where toTuple [a, b] = Tuple a b
          toTuple _ = Tuple "" ""

deleteSession :: forall eff. Eff (Effects eff) Unit
deleteSession = WebStorage.removeItem WebStorage.localStorage "session"

getState :: forall uid userdata eff err. (ToJSON userdata, ToJSON err) => Config userdata err -> Aff (Effects eff) (Maybe (State uid userdata err))
getState props = do
  window <- liftEff $ DOM.window
  location <- liftEff $ DOM.location window
  origin <- liftEff $ DOM.origin location
  pathname <- liftEff $ DOM.pathname location
  hash <- liftEff $ DOM.hash location
  search <- liftEff $ DOM.search location

  let -- specialized sendSync
      sendSync :: forall b. (FromJSON b) => S.Socket -> (R.Proxy b -> UserCommand userdata err) -> Aff (Effects eff) (Either Error b)
      sendSync = R.sendSync

  if hash == "#" ++ props.redirectUrl
    then do
      token <- sendSync props.socket (RPC.AuthFacebook (origin ++ pathname ++ "#" ++ props.redirectUrl) (parseParams search) props.sessionLength)

      case token of
        Left _ -> return Nothing
        Right (Right sessionId) -> do
          with <- withSessionId true sessionId
          return $ Just (with emptyState)
    else do
      session <- liftEff $ WebStorage.getItem WebStorage.localStorage "session"
      case session of
        Just session -> return $ Just $ emptyState { sessionId = Just $ RPC.SessionId { unSessionId: session } }
        Nothing -> return Nothing
