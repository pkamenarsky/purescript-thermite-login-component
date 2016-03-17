module Thermite.Login.Model where

import Control.Alt ((<|>))
import Control.Apply ((<*))

import Data.Functor ((<$))
import Data.Lens
import Data.Either
import Data.Maybe
import Data.Tuple (Tuple)

import Routing.Match as Routing
import Routing.Match.Class as Routing

import DOM.HTML.Types as DOM

import Prelude
import Network.WebSockets.Sync.Socket as S
import Web.Users.Remote.Types.Shared as RPC

data Action uid userdata err =
    Login
  | LoginWithFacebook
  | Logout
  | Register
  | ResetPassword

  | ChangeScreen Screen
  | ScreenChanged Screen
  | TextChanged (Lens (State uid userdata err) (State uid userdata err) String String) String

data Screen = LoginScreen | RegisterScreen | ResetPasswordScreen

type Locale err field =
  { name :: String
  , password :: String
  , repeatPassword :: String
  , resetPassword :: String
  , login :: String
  , email :: String
  , forgotPassword :: String
  , register :: String
  , loginWithFacebook :: String

  , errUserOrPasswordIncorrect :: String
  , errUserOrEmailAlreadyTaken :: String

  , userCreatedSuccessfully :: String

  , userDataValidationError :: err -> String

  , additionalFieldTitle :: field -> String
  }

type Validator st = st -> Boolean

hoistValidator :: forall st1 st2. Lens st2 st2 st1 st1 -> Validator st1 -> Validator st2
hoistValidator lens v st = v $ view lens st

type Field userdata field =
  { field :: field
  -- FIXME: waiting on https://github.com/purescript/purescript/issues/1957
  -- , fieldLens :: Lens userdata userdata String String
  , fieldLens :: Tuple (userdata -> String) (userdata -> String -> userdata)
  , validate :: Validator userdata
  }

type Config userdata err field =
  { redirectUrl :: String
  , socket :: S.Socket
  , sessionLength :: Int
  , defaultUserData :: userdata
  , locale :: Locale err field
  , additionalFields :: Array (Field userdata field)
  }

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
  , resetLoading :: Boolean
  }

emptyResetPasswordState :: ResetPasswordState
emptyResetPasswordState =
  { resetEmail: ""
  , resetLoading: false
  }

type State uid userdata err =
  { sessionId :: Maybe RPC.SessionId
  , redirectingAfterLogin :: Boolean
  , screen :: Screen
  , regState :: RegisterState userdata err
  , loginState :: LoginState
  , resetPasswordState :: ResetPasswordState
  }

emptyState :: forall uid userdata err. userdata -> State uid userdata err
emptyState userdata =
  { sessionId: Nothing
  , screen: LoginScreen
  , redirectingAfterLogin: false
  , regState: emptyRegisterState userdata
  , loginState: emptyLoginState
  , resetPasswordState: emptyResetPasswordState
  }

-- Locales

localeDe :: forall err field. (err -> String) -> (field -> String) -> Locale err field
localeDe userDataValidationError additionalFieldTitle =
  { name: "Username"
  , password: "Passwort"
  , repeatPassword: "Passwort wiederholen"
  , resetPassword: "Neusetzen"
  , login: "Einloggen"
  , email: "Email"
  , forgotPassword: "Passwort vergessen"
  , register: "Registrieren"
  , loginWithFacebook: "Mit Facebook einloggen"

  , errUserOrPasswordIncorrect: "Username/Passwort falsch"
  , errUserOrEmailAlreadyTaken: "Username/Email existieren schon"

  , userCreatedSuccessfully: "User erfolgreich registriert"
  , userDataValidationError
  , additionalFieldTitle
  }

toRoute :: Screen -> String
toRoute LoginScreen = ""
toRoute RegisterScreen = "register"
toRoute ResetPasswordScreen = "reset"

loginMatch :: Routing.Match Screen
loginMatch = LoginScreen <$ Routing.lit ""
         <|> RegisterScreen <$ Routing.lit "register"
         <|> ResetPasswordScreen <$ Routing.lit "reset"
