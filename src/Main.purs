module Main where

import Prelude

import Data.JSON
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (throwException, EXCEPTION())

import qualified Network.WebSockets.Sync.Socket as S
import qualified Network.WebSockets.Sync.Request as R

import qualified Web.Users.Remote.Types.Shared as RPC

import qualified Thermite as T
import qualified Thermite.Aff as T

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.ParentNode as DOM

import qualified React as R
import qualified React.DOM as R
-- import qualified React.DOM.Props as RP

type UserCommand = RPC.UserCommand String String String

-- specialized sendSync
sendSync :: forall eff b. (FromJSON b) => S.Socket -> (R.Proxy b -> UserCommand) -> Aff (websocket :: S.WebSocket | eff) b
sendSync = R.sendSync

-- Action
data Action = NoOp

type State = { session :: String, socket :: S.Socket }

render :: T.Render State _ Action
render dispatch _ state _ = [ R.div [] [ R.text "asdasd" ] ]

performAction :: T.PerformAction _ State _ Action
performAction = T.asyncOne' handler
  where
    handler :: Action -> _ -> State -> Aff _ (State -> State)
    handler NoOp _ state = do
      url <- sendSync state.socket (RPC.AuthFacebookUrl "" [])
      pure $ \x -> x

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main :: forall e. Eff (dom :: DOM.DOM, websocket :: S.WebSocket, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  socket <- S.connect "ws://localhost:8538" false
    { connected : \_ -> return unit
    , disconnected : return unit
    , message : \_ -> return unit
    }

  runAff throwException (const (pure unit)) $ do
    -- TODO: reopen connection on send
    -- url <- sendSync socket (RPC.AuthFacebookUrl "" [])
    user <- sendSync socket $ RPC.CreateUser (RPC.User
      { u_name: "user"
      , u_email: "email"
      , u_password: RPC.PasswordHidden
      , u_active: true
      , u_more: "none" }) "SECRET!!!"
    return unit

  let component = T.createClass spec { session : "", socket }

  document <- DOM.window >>= DOM.document
  container <- fromJust <<< toMaybe <$> DOM.querySelector "#main" (DOM.htmlDocumentToParentNode document)

  R.render (R.createFactory component {}) container
  return unit
