module Model where

import Data.Lens

import Prelude
import qualified Network.WebSockets.Sync.Socket as S

data Action =
  NoOp
  | Login
  | TextChanged (Lens State State String String) String

data Screen = LoginScreen | RegisterScreen

type State =
  { session :: String
  , username :: String
  , password :: String
  , socket :: S.Socket
  , facebookUrl :: String

  , screen :: Screen
  , regName :: String
  , regEmail :: String
  , regPassword :: String
  , regRepeatPassword :: String
  }

