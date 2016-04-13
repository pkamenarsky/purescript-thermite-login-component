module Thermite.Login.Masks where

import Data.Array (concat)
import Data.Either
import Data.Foldable (and)
import Data.Lens
import Data.Maybe
import Data.String (null)
import Data.Tuple

import React as R
import React.DOM as R
import React.DOM.Props as RP
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

validateEmail :: forall userdata err. Validator (RegisterState userdata err)
validateEmail st = not null st.regEmail

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
                -> Validator st
                -> Maybe action
                -> String
                -> LensP st String
                -> _
renderTextinput' dispatch props state onChange pwd validate onEnter text lens =
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
    ] []
  ]

renderTextinput dispatch props state onChange = renderTextinput' dispatch props state onChange false validateAlways Nothing

--------------------------------------------------------------------------------

data RegisterAction userdata err =
  RegisterTextChanged (RegisterState userdata err -> RegisterState userdata err)
  | RegisterRegister

type RegisterConfig err field = { locale :: Locale err field }

registerMask :: forall uid userdata err field eff. T.Spec eff (RegisterState userdata err) (RegisterConfig err field) (RegisterAction userdata err)
registerMask = T.simpleSpec performAction render
  where
  performAction action props state = return unit

  render dispatch props state _ =
    let textinput' = renderTextinput' dispatch props state RegisterTextChanged
        textinput = renderTextinput dispatch props state (unsafeCoerce unit)
        in concat
          [ textinput props.locale.name regName
          , textinput props.locale.email regEmail
          , textinput' true validateAlways Nothing props.locale.password regPassword
          , textinput' true (validateRepeatPassword regPassword regRepeatPassword) (Just RegisterRegister) props.locale.repeatPassword regRepeatPassword
          , [ button
                ( allValid state
                  $ [ validateEmail
                    , validateRepeatPassword regPassword regRepeatPassword
                    ]
                )
                state.regLoading
                props.locale.register
                "login-button-register"
                dispatch
                RegisterRegister
            ]
          , case state.regResult of
              Just (Left (RPC.CreateUserValidationError err)) -> [ R.div [ RP.className "login-register-error" ] [ R.text $ props.locale.userDataValidationError err ] ]
              Just (Left _) -> [ R.div [ RP.className "login-register-error" ] [ R.text props.locale.errUserOrEmailAlreadyTaken ] ]
              Just (Right _) ->  [ R.div [ RP.className "login-register-success" ] [ R.text props.locale.userCreatedSuccessfully ] ]
              _ ->  []
          ]
