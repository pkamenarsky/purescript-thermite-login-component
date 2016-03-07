module Thermite.Login.Model where

import Control.Alt ((<|>))
import Control.Apply ((<*))

import Data.Functor ((<$))
import Data.Lens
import Data.Either
import Data.Maybe

import Routing.Match as Routing
import Routing.Match.Class as Routing

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
  | ScreenChanged Screen
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
  , regLoading :: Boolean

  , regResult :: Maybe (Either RPC.CreateUserExtraError Unit)
  }

emptyRegisterState :: RegisterState
emptyRegisterState =
  { regName: ""
  , regFullName: ""
  , regEmail: ""
  , regPassword: ""
  , regRepeatPassword: ""
  , regLoading: false

  , regResult: Nothing
  }

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

type ResetPasswordState =
  { resetEmail :: String
  }

emptyResetPasswordState :: ResetPasswordState
emptyResetPasswordState =
  { resetEmail: ""
  }

type State uid userdata =
  { sessionId :: Maybe RPC.SessionId
  , redirectingAfterLogin :: Boolean
  , screen :: Screen
  , regState :: RegisterState
  , loginState :: LoginState
  , resetPasswordState :: ResetPasswordState
  }

emptyState :: forall uid userdata. State uid userdata
emptyState =
  { sessionId: Nothing
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
  , repeatPassword: "Passwort wiederholen"
  , resetPassword: "Neusetzen"
  , login: "Einloggen"
  , fullName: "Name"
  , email: "Email"
  , forgotPassword: "Passwort vergessen"
  , register: "Registrieren"
  , loginWithFacebook: "Mit Facebook einloggen"

  , errUserOrPasswordIncorrect: "Username oder Passwort falsch"
  , errUserOrEmailAlreadyTaken: "Username oder Email existieren schon"
  , errEmptyFullname: "Name darf nicht leer sein"

  , userCreatedSuccessfully: "User erfolgreich registriert"
  }

toRoute :: Screen -> String
toRoute LoginScreen = ""
toRoute RegisterScreen = "register"
toRoute ResetPasswordScreen = "reset"

loginMatch :: Routing.Match Screen
loginMatch = LoginScreen <$ Routing.lit ""
         <|> RegisterScreen <$ Routing.lit "register"
         <|> ResetPasswordScreen <$ Routing.lit "reset"
