module Thermite.Login.Model where

import Data.Lens
import Data.Either
import Data.Maybe

import DOM.HTML.Types as DOM

import Prelude
import Network.WebSockets.Sync.Socket as S
import Web.Users.Remote.Types.Shared as RPC

data Action userdata =
    Login
  | Register
  | ResetPassword

  | ChangeScreen Screen
  | TextChanged (Lens (State userdata) (State userdata) String String) String

data Screen = LoginScreen | RegisterScreen | ResetPasswordScreen

type Config userdata =
  { redirectUrl :: String
  , facebookLoginUrl :: String
  , socket :: S.Socket
  , sessionLength :: Int
  , defaultUserData :: userdata
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

type State userdata =
  { sessionId :: Maybe RPC.SessionId
  , sessionUser :: Maybe (RPC.User userdata)
  , redirectingAfterLogin :: Boolean
  , screen :: Screen
  , regState :: RegisterState
  , loginState :: LoginState
  , resetPasswordState :: ResetPasswordState
  }

emptyState :: forall userdata. State userdata
emptyState =
  { sessionId: Nothing
  , sessionUser: Nothing
  , screen: LoginScreen
  , redirectingAfterLogin: false
  , regState: emptyRegisterState
  , loginState: emptyLoginState
  , resetPasswordState: emptyResetPasswordState
  }
