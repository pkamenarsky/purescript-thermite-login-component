module Thermite.Login.Model where

import Data.Lens
import Data.Either
import Data.Maybe

import DOM.HTML.Types as DOM

import Prelude
import Network.WebSockets.Sync.Socket as S
import Web.Users.Remote.Types.Shared as RPC

data Action uid userdata =
    Login
  | LoginWithFacebook
  | Logout
  | Register
  | ResetPassword

  | ChangeScreen Screen
  | TextChanged (Lens (State uid userdata) (State uid userdata) String String) String

data Screen = LoginScreen | RegisterScreen | ResetPasswordScreen

type Locale =
  { name :: String
  , password :: String
  , repeatPassword :: String
  , resetPassword :: String
  , login :: String
  , fullName :: String
  , email :: String
  , forgotPassword :: String
  , register :: String
  , loginWithFacebook :: String

  , errUserOrPasswordIncorrect :: String
  , errUserOrEmailAlreadyTaken :: String
  , errEmptyFullname :: String

  , userCreatedSuccessfully :: String
  }

type Config userdata =
  { redirectUrl :: String
  , socket :: S.Socket
  , sessionLength :: Int
  , defaultUserData :: userdata
  , locale :: Locale
  }

type RegisterState =
  { regName :: String
  , regFullName :: String
  , regEmail :: String
  , regPassword :: String
  , regRepeatPassword :: String

  , regResult :: Maybe (Either RPC.CreateUserExtraError Unit)
  }

emptyRegisterState :: RegisterState
emptyRegisterState =
  { regName: ""
  , regFullName: ""
  , regEmail: ""
  , regPassword: ""
  , regRepeatPassword: ""

  , regResult: Nothing
  }

type LoginState =
  { loginName :: String
  , loginPassword :: String
  , loginError :: Boolean
  }

emptyLoginState :: LoginState
emptyLoginState =
  { loginName: ""
  , loginPassword: ""
  , loginError: false
  }

type ResetPasswordState =
  { resetEmail :: String
  }

emptyResetPasswordState :: ResetPasswordState
emptyResetPasswordState =
  { resetEmail: ""
  }

type State uid userdata =
  { sessionId :: Maybe RPC.SessionId
  , userId :: Maybe uid
  , sessionUser :: Maybe (RPC.User (RPC.UserAdditionalInfo userdata))
  , redirectingAfterLogin :: Boolean
  , screen :: Screen
  , regState :: RegisterState
  , loginState :: LoginState
  , resetPasswordState :: ResetPasswordState
  }

emptyState :: forall uid userdata. State uid userdata
emptyState =
  { sessionId: Nothing
  , sessionUser: Nothing
  , userId: Nothing
  , screen: LoginScreen
  , redirectingAfterLogin: false
  , regState: emptyRegisterState
  , loginState: emptyLoginState
  , resetPasswordState: emptyResetPasswordState
  }

-- Locales

localeDe =
  { name: "Username"
  , password: "Passwort"
  , repeatPassword: "Password wiederholen"
  , resetPassword: "Password neusetzen"
  , login: "Einloggen"
  , fullName: "Name"
  , email: "Email"
  , forgotPassword: "Passwort vergessen"
  , register: "Registrieren"
  , loginWithFacebook: "Mit Facebook einloggen"

  , errUserOrPasswordIncorrect: "Username oder Password falsch"
  , errUserOrEmailAlreadyTaken: "Username oder Email existieren schon"
  , errEmptyFullname: "Name darf nicht leer sein"

  , userCreatedSuccessfully: "User erfolgreich registriert"
  }
