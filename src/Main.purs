module Main where

import Prelude

import Data.JSON

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (throwException, EXCEPTION())

import qualified Network.WebSockets.Sync.Socket as S
import qualified Network.WebSockets.Sync.Request as R

import qualified Web.Users.Remote.Types.Shared as RPC

type UserCommand = RPC.UserCommand String String String

-- specialized sendSync
sendSync :: forall eff b. (FromJSON b) => S.Socket -> (R.Proxy b -> UserCommand) -> Aff (websocket :: S.WebSocket | eff) b
sendSync = R.sendSync

main :: forall e. Eff (websocket :: S.WebSocket, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  socket <- S.connect "ws://localhost:8000"
    { connected : \_ -> return unit
    , disconnected : return unit
    , message : \_ -> return unit
    }

  runAff throwException (const (pure unit)) $ do
    url <- sendSync socket (RPC.AuthFacebookUrl "" [])
    return url

  log ""
