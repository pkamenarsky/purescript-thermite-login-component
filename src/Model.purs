module Model where

import Data.Lens

import Prelude
import qualified Network.WebSockets.Sync.Socket as S

data Action =
  NoOp
  | Login
  | TextChanged (Lens State State String String) String

data Screen = LoginScreen | RegisterScreen | ResetPasswordScreen

type RegisterState =
  { regName :: String
  , regEmail :: String
  , regPassword :: String
  , regRepeatPassword :: String
  }

emptyRegisterState :: RegisterState
emptyRegisterState =
  { regName: ""
  , regEmail: ""
  , regPassword: ""
  , regRepeatPassword: ""
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
  { session :: String
  , socket :: S.Socket

  , facebookUrl :: String

  , screen :: Screen
  , regState :: RegisterState
  , loginState :: LoginState
  , resetPasswordState :: ResetPasswordState
  }

emptyState :: S.Socket -> String -> State
emptyState socket facebookUrl =
  { session: ""
  , socket
  , facebookUrl
  , screen: LoginScreen
  , regState: emptyRegisterState
  , loginState: emptyLoginState
  , resetPasswordState: emptyResetPasswordState
  }
