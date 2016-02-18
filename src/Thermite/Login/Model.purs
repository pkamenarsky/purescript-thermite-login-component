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

  , loginSession :: Maybe (Either Unit RPC.SessionId)
  }

emptyLoginState :: LoginState
emptyLoginState =
  { loginName: ""
  , loginPassword: ""

  , loginSession: Nothing
  }

type ResetPasswordState =
  { resetEmail :: String
  }

emptyResetPasswordState :: ResetPasswordState
emptyResetPasswordState =
  { resetEmail: ""
  }

type State =
  { session :: Maybe RPC.SessionId
  , socket :: S.Socket

  , facebookUrl :: String

  , screen :: Screen
  , regState :: RegisterState
  , loginState :: LoginState
  , resetPasswordState :: ResetPasswordState
  }

emptyState :: S.Socket -> String -> State
emptyState socket facebookUrl =
  { session: Nothing
  , socket
  , facebookUrl
  , screen: LoginScreen
  , regState: emptyRegisterState
  , loginState: emptyLoginState
  , resetPasswordState: emptyResetPasswordState
  }
