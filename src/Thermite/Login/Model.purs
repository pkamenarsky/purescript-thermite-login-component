module Thermite.Login.Model where

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Alt ((<|>))
import Control.Apply ((<*))
import Control.Monad.Eff.Exception (Error)

import Data.Functor ((<$))
import Data.JSON (JValue)
import Data.Lens
import Data.Either
import Data.Maybe
import Data.Tuple (Tuple)

import Routing.Match as Routing
import Routing.Match.Class as Routing

import DOM.HTML.Types as DOM

import React as R
import Thermite as T

import Prelude
import Network.WebSockets.Sync.Socket as S
import Web.Users.Remote.Types.Shared as RPC


type UserCommand uid userdata err = RPC.UserCommand userdata uid RPC.SessionId err

data Action uid userdata err =
    Logout
  | SubRegisterAction (RegisterAction userdata err)
  | SubLoginAction LoginAction
  | SubResetAction ResetAction
  | SubSetNewPasswordAction SetNewPasswordAction

  | ChangeScreen Screen

data Screen =
    LoginScreen
  | RegisterScreen
  | ResetPasswordScreen
  | SetNewPasswordScreen String

type Locale custom =
  { name :: String
  , password :: String
  , repeatPassword :: String
  , resetPassword :: String
  , login :: String
  , email :: String
  , forgotPassword :: String
  , register :: String
  , setPassword :: String
  , loginWithFacebook :: String

  , errUserOrPasswordIncorrect :: String
  , errUserOrEmailAlreadyTaken :: String

  , userCreatedSuccessfully :: String

  , passwordResetMailSentSuccessfully :: String

  , newPasswordSetSuccessfully :: String

  , custom :: custom
  }

type Validator st = st -> Boolean

hoistValidator :: forall st1 st2. Lens st2 st2 st1 st1 -> Validator st1 -> Validator st2
hoistValidator lens v st = v $ view lens st

type Config uid userdata err custom eff =
  { redirectUrl :: String
  , redirectToScreen :: Screen -> Eff eff Unit
  , sendRequest :: UserCommand uid userdata err -> Aff eff (Either Error JValue)
  , sessionLength :: Int
  , defaultUserData :: userdata
  , locale :: Locale custom

  , registerMask :: T.Spec eff (RegisterState userdata err) (RegisterConfig custom) (RegisterAction userdata err)
  , loginMask :: T.Spec eff LoginState (LoginConfig custom) LoginAction
  , resetPasswordMask :: T.Spec eff ResetPasswordState (ResetConfig custom) ResetAction
  , setNewPasswordMask :: T.Spec eff SetNewPasswordState (SetNewPasswordConfig custom) SetNewPasswordAction
  }

-- Register mask ---------------------------------------------------------------

data RegisterAction userdata err =
    RegisterTextChanged (RegisterState userdata err -> RegisterState userdata err)
  | Register

type RegisterConfig custom = { locale :: Locale custom }

type RegisterState userdata err =
  { regName :: String
  , regEmail :: String
  , regPassword :: String
  , regRepeatPassword :: String
  , regLoading :: Boolean
  , regUserData :: userdata

  , regResult :: Maybe (Either (RPC.CreateUserValidationError err) Unit)
  }

emptyRegisterState :: forall userdata err. userdata -> RegisterState userdata err
emptyRegisterState regUserData =
  { regName: ""
  , regEmail: ""
  , regPassword: ""
  , regRepeatPassword: ""
  , regLoading: false
  , regUserData

  , regResult: Nothing
  }

-- Login mask ------------------------------------------------------------------

data LoginAction =
    LoginTextChanged (LoginState -> LoginState)
  | Login
  | LoginWithFacebook
  | LoginScreenChanged Screen

type LoginConfig custom = { locale :: Locale custom }

type LoginState =
  { loginName :: String
  , loginPassword :: String
  , loginError :: Boolean
  , loginLoading :: Boolean
  }

emptyLoginState :: LoginState
emptyLoginState =
  { loginName: ""
  , loginPassword: ""
  , loginError: false
  , loginLoading: false
  }

-- Reset passwor mask ----------------------------------------------------------

data ResetAction =
    ResetTextChanged (ResetPasswordState -> ResetPasswordState)
  | ResetPassword

type ResetConfig custom = { locale :: Locale custom }

type ResetPasswordState =
  { resetEmail :: String
  , resetLoading :: Boolean
  , resetShowSuccessMessage :: Boolean
  }

emptyResetPasswordState :: ResetPasswordState
emptyResetPasswordState =
  { resetEmail: ""
  , resetLoading: false
  , resetShowSuccessMessage: false
  }

-- Set new password mask -------------------------------------------------------

data SetNewPasswordAction =
    SetNewPasswordTextChanged (SetNewPasswordState -> SetNewPasswordState)
  | SetNewPassword String

type SetNewPasswordConfig custom = { locale :: Locale custom, token :: String }

type SetNewPasswordState =
  { setpwdPassword :: String
  , setpwdRepeatPassword :: String
  , setpwdShowSuccessMessage :: Boolean
  , setpwdLoading :: Boolean
  }

emptySetNewPasswordState :: SetNewPasswordState
emptySetNewPasswordState =
  { setpwdPassword: ""
  , setpwdRepeatPassword: ""
  , setpwdShowSuccessMessage: false
  , setpwdLoading: false
  }

-- Global state ----------------------------------------------------------------

type State uid userdata err =
  { sessionId :: Maybe RPC.SessionId
  , redirectingAfterLogin :: Boolean
  , screen :: Screen
  , regState :: RegisterState userdata err
  , loginState :: LoginState
  , resetPasswordState :: ResetPasswordState
  , setNewPasswordState :: SetNewPasswordState
  }

emptyState :: forall uid userdata err. userdata -> State uid userdata err
emptyState userdata =
  { sessionId: Nothing
  , screen: LoginScreen
  , redirectingAfterLogin: false
  , regState: emptyRegisterState userdata
  , loginState: emptyLoginState
  , resetPasswordState: emptyResetPasswordState
  , setNewPasswordState: emptySetNewPasswordState
  }

-- Locales ---------------------------------------------------------------------

localeDe :: forall custom. custom -> Locale custom
localeDe custom =
  { name: "Username"
  , password: "Passwort"
  , repeatPassword: "Passwort wiederholen"
  , resetPassword: "Neusetzen"
  , login: "Einloggen"
  , email: "Email"
  , forgotPassword: "Passwort vergessen"
  , register: "Registrieren"
  , setPassword: "Passwort setzen"
  , loginWithFacebook: "Mit Facebook einloggen"

  , errUserOrPasswordIncorrect: "Username/Passwort falsch"
  , errUserOrEmailAlreadyTaken: "Username/Email existieren schon"

  , userCreatedSuccessfully: "User erfolgreich registriert"
  , passwordResetMailSentSuccessfully: "Mail wurde versandt"
  , newPasswordSetSuccessfully: "Passwort wurde gesetzt"

  , custom
  }

-- Routing ---------------------------------------------------------------------

toRoute :: Screen -> String
toRoute LoginScreen = ""
toRoute RegisterScreen = "register"
toRoute ResetPasswordScreen = "reset"
toRoute (SetNewPasswordScreen _) = "setpwd"

loginMatch :: Routing.Match Screen
loginMatch = LoginScreen <$ Routing.lit ""
         <|> RegisterScreen <$ Routing.lit "register"
         <|> ResetPasswordScreen <$ Routing.lit "reset"
         <|> SetNewPasswordScreen <$ Routing.lit "setpwd" <*> Routing.str
