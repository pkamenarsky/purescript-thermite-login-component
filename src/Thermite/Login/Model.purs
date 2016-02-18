module Thermite.Login.Model where

import Data.Lens
import Data.Either
import Data.Maybe

import Prelude
import Network.WebSockets.Sync.Socket as S
import Web.Users.Remote.Types.Shared as RPC

data Action =
    Login
  | Register
  | ResetPassword

  | ChangeScreen Screen
  | TextChanged (Lens State State String String) String

data Screen = LoginScreen | RegisterScreen | ResetPasswordScreen

type Config =
  { redirectUrl :: String
  , socket :: S.Socket
  , sessionLength :: Int
  }

type RegisterState =
  { regName :: String
  , regEmail :: String
  , regPassword :: String
  , regRepeatPassword :: String

  , regResult :: Maybe (Either Unit Unit)
  }

emptyRegisterState :: RegisterState
emptyRegisterState =
  { regName: ""
  , regEmail: ""
  , regPassword: ""
  , regRepeatPassword: ""

  , regResult: Nothing
  }

type LoginState =
  { loginName :: String
  , loginPassword :: String
  }

emptyLoginState :: LoginState
emptyLoginState =
  { loginName: ""
  , loginPassword: ""
  }

type ResetPasswordState =
  { resetEmail :: String
  }

emptyResetPasswordState :: ResetPasswordState
emptyResetPasswordState =
  { resetEmail: ""
  }

type State =
  { sessionId :: Maybe RPC.SessionId

  , facebookLoginUrl :: String

  , screen :: Screen
  , regState :: RegisterState
  , loginState :: LoginState
  , resetPasswordState :: ResetPasswordState
  }

emptyState :: String -> State
emptyState facebookLoginUrl =
  { sessionId: Nothing
  , screen: LoginScreen
  , facebookLoginUrl
  , regState: emptyRegisterState
  , loginState: emptyLoginState
  , resetPasswordState: emptyResetPasswordState
  }
