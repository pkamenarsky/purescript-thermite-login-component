module Thermite.Login.Masks where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Data.Array (concat)
import Data.Either
import Data.Foldable (and)
import Data.JSON (JValue)
import Data.Lens
import Data.Maybe
import Data.String (null)
import Data.Tuple

import Text.Email.Validate as Email

import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite (modify, get)
import Thermite as T

import Unsafe.Coerce (unsafeCoerce)

import Prelude

import Web.Users.Remote.Types.Shared as RPC

import Thermite.Login.Model
import Thermite.Login.Model.Lenses

allValid :: forall st. st -> Array (Validator st) -> Boolean
allValid st = and <<< map ($ st)

validateAlways :: forall st. Validator st
validateAlways _ = true

validateEmail :: forall st. LensP st String -> Validator st
validateEmail email st = not (null $ st ^. email) && Email.isValid (st ^. email)

validateRepeatPassword :: forall st.
                          LensP st String
                       -> LensP st String
                       -> Validator st
validateRepeatPassword pw1 pw2 st = not null (st ^. pw1) && st ^. pw1 == st ^. pw2

button :: forall action. Boolean -> Boolean -> String -> String -> (action -> T.EventHandler) -> action -> R.ReactElement
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

renderTextinput' :: forall st props action.
                   (action -> T.EventHandler)
                -> props
                -> st
                -> ((st -> st) -> action)
                -> Boolean
                -> Boolean
                -> Validator st
                -> Maybe action
                -> String
                -> LensP st String
                -> _
renderTextinput' dispatch props state onChange autofocus pwd validate onEnter text lens =
  [ R.input
    [ RP.onChange \e -> dispatch $ onChange (set lens ((unsafeCoerce e).target.value))
    , RP.onKeyDown \e -> case onEnter of
        Just action | e.keyCode == 13 -> dispatch action
        _ -> return unit
    , RP.value (state ^. lens)
    , RP.placeholder text
    , if validate state
         then RP.className "login-input"
         else RP.className "login-input login-input-error"
    , if pwd then RP._type "password" else RP._type ""
    , RP.autoFocus autofocus
    , RP.autoComplete "off"
    ] []
  ]

renderTextinput dispatch props state onChange = renderTextinput' dispatch props state onChange false false validateAlways Nothing

--------------------------------------------------------------------------------

registerMask :: forall userdata err custom eff. T.Spec eff (RegisterState userdata err) (RegisterConfig custom) (RegisterAction userdata err)
registerMask = T.simpleSpec performAction render
  where
  performAction (RegisterTextChanged f) props state = modify f
  performAction Register props state = return unit

  render dispatch props state _ = concat
    [ textinput props.locale.name regName
    , textinput props.locale.email regEmail
    , textinput' false true validateAlways Nothing props.locale.password regPassword
    , textinput' false true (validateRepeatPassword regPassword regRepeatPassword) (Just Register) props.locale.repeatPassword regRepeatPassword
    , [ button
          ( allValid state
            $ [ validateEmail regEmail
              , validateRepeatPassword regPassword regRepeatPassword
              ]
          )
          state.regLoading
          props.locale.register
          "login-button-register"
          dispatch
          Register
      ]
    , case state.regResult of
        Just (Left _) -> [ R.div [ RP.className "login-register-error" ] [ R.text props.locale.errUserOrEmailAlreadyTaken ] ]
        Just (Right _) ->  [ R.div [ RP.className "login-register-success" ] [ R.text props.locale.userCreatedSuccessfully ] ]
        _ ->  []
    ]
    where
    textinput' = renderTextinput' dispatch props state RegisterTextChanged
    textinput = renderTextinput dispatch props state RegisterTextChanged


--------------------------------------------------------------------------------

loginMask :: forall custom eff. T.Spec eff LoginState (LoginConfig custom) LoginAction
loginMask = T.simpleSpec performAction render
  where
  performAction (LoginTextChanged f) props state = modify f
  performAction Login props state = return unit
  performAction LoginWithFacebook props state = return unit
  performAction (LoginScreenChanged _) props state = return unit

  render dispatch props state _ = concat
    [ textinput props.locale.name loginName
    , textinput' false true validateAlways (if state.loginLoading then Nothing else Just Login) props.locale.password loginPassword
    , [ R.div
        [ RP.className "login-text-container" ]
        [ R.div
          [ RP.onClick \_ -> dispatch (LoginScreenChanged ResetPasswordScreen)
          , RP.className "login-text-forgot-password"
          ]
          [ R.text props.locale.forgotPassword ]
        , R.div
          [ RP.onClick \_ -> dispatch (LoginScreenChanged RegisterScreen)
          , RP.className "login-text-register"
          ]
          [ R.text props.locale.register ]
        ]
      , button
          true
          state.loginLoading
          props.locale.login
          "login-button-login"
          dispatch
          Login
      , R.div [ RP.className "login-divider" ] []
      {-
      , R.div
        [ RP.onClick \_ -> dispatch LoginWithFacebook
        , RP.className "login-button-facebook"
        ]
        [ R.text props.locale.loginWithFacebook ]
      -}
      ]
    , if state.loginError
         then [ R.div
                [ RP.className "login-error" ]
                [ R.text props.locale.errUserOrPasswordIncorrect ]
              ]
         else [ ]
    ]
    where
    textinput' = renderTextinput' dispatch props state LoginTextChanged
    textinput = renderTextinput dispatch props state LoginTextChanged

--------------------------------------------------------------------------------

resetPasswordMask :: forall userdata err custom eff. T.Spec eff ResetPasswordState (ResetConfig custom) ResetAction
resetPasswordMask = T.simpleSpec performAction render
  where
  performAction (ResetTextChanged f) props state = modify f
  performAction ResetPassword props state = return unit

  render dispatch props state _ = concat
    [ textinput' false false validateAlways (Just ResetPassword) props.locale.email resetEmail
    , [ button
          true
          state.resetLoading
          props.locale.resetPassword
          "login-button-reset-password"
          dispatch
          ResetPassword
      ]
    , if state.resetShowSuccessMessage
        then [ R.div [ RP.className "login-register-success" ] [ R.text $ props.locale.passwordResetMailSentSuccessfully ] ]
        else []
    ]
    where
    textinput' = renderTextinput' dispatch props state ResetTextChanged
    textinput = renderTextinput dispatch props state ResetTextChanged

--------------------------------------------------------------------------------

setNewPasswordMask :: forall userdata err custom eff. T.Spec eff SetNewPasswordState (SetNewPasswordConfig custom) SetNewPasswordAction
setNewPasswordMask = T.simpleSpec performAction render
  where
  performAction (SetNewPasswordTextChanged f) props state = modify f
  performAction (SetNewPassword _) props state = return unit

  render dispatch props state _ = concat
    [ textinput' false true validateAlways Nothing props.locale.password setpwdPassword
    , textinput' false true (validateRepeatPassword setpwdPassword setpwdRepeatPassword) (Just $ SetNewPassword props.token) props.locale.repeatPassword setpwdRepeatPassword
    , [ button
          (validateRepeatPassword setpwdPassword setpwdRepeatPassword state)
          state.setpwdLoading
          props.locale.setPassword
          "login-button-register"
          dispatch
          (SetNewPassword props.token)
      ]
    , if state.setpwdShowSuccessMessage
        then [ R.div [ RP.className "login-register-success" ] [ R.text $ props.locale.newPasswordSetSuccessfully ] ]
        else []
    ]
    where
    textinput' = renderTextinput' dispatch props state SetNewPasswordTextChanged
    textinput = renderTextinput dispatch props state SetNewPasswordTextChanged

--------------------------------------------------------------------------------

defaultConfig :: forall uid userdata custom eff err.
  { redirectUrl :: String
  , redirectToScreen :: Screen -> Eff eff Unit
  , sendRequest :: UserCommand uid userdata err -> Aff eff (Either Error JValue)
  , sessionLength :: Int
  , defaultUserData :: userdata
  , locale :: Locale custom
  } -> Config uid userdata err custom eff
defaultConfig cfg =
  { registerMask
  , loginMask
  , resetPasswordMask
  , setNewPasswordMask

  , redirectUrl: cfg.redirectUrl
  , redirectToScreen: cfg.redirectToScreen
  , sendRequest: cfg.sendRequest
  , sessionLength: cfg.sessionLength
  , defaultUserData: cfg.defaultUserData
  , locale: cfg.locale
  }
