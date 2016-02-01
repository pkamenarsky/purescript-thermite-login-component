module Main where

import Prelude

import Data.Array (concat)
import Data.JSON
import Data.Lens
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (throwException, EXCEPTION())

import Network.WebSockets.Sync.Socket as S
import Network.WebSockets.Sync.Request as R

import Web.Users.Remote.Types.Shared as RPC

import Thermite as T
import Thermite.Aff as T

import DOM as DOM
import DOM.HTML as DOM
import DOM.HTML.Types as DOM
import DOM.HTML.Window as DOM
import DOM.Node.ParentNode as DOM

import React as R
import React.DOM as R
import React.DOM.Props as RP

import Unsafe.Coerce

import Model
import Model.Lenses

type UserCommand = RPC.UserCommand String String String

-- specialized sendSync
sendSync :: forall eff b. (FromJSON b) => S.Socket -> (R.Proxy b -> UserCommand) -> Aff (websocket :: S.WebSocket | eff) b
sendSync = R.sendSync

renderRegisterScreen :: T.Render State _ Action
renderRegisterScreen dispatch _ state _ = concat
  [ textinput "Name" (regState <<< regName)
  , textinput "Email" (regState <<< regEmail)
  , textinput "Password" (regState <<< regPassword)
  , textinput "Repeat password" (regState <<< regRepeatPassword)
  ]
  where
    textinput :: String -> Lens State State String String -> _
    textinput text lens =
      [ R.div [] [ R.text text ]
      , R.input
        [ RP.onChange \e -> dispatch $ TextChanged lens ((unsafeCoerce e).target.value)
        , RP.value (state ^. lens)
        ] []
      ]

render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.input
    [ RP.onChange \e -> dispatch $ TextChanged (loginState <<< loginName) ((unsafeCoerce e).target.value)
    , RP.value state.loginState.loginName
    ] []
  , R.input
    [ RP.onChange \e -> dispatch $ TextChanged (loginState <<< loginPassword) ((unsafeCoerce e).target.value)
    , RP.value state.loginState.loginPassword
    ] []
  , R.div
    [ RP.onClick \_ -> dispatch Login ]
    [ R.text "Login" ]
  , R.div
    [ RP.onClick \_ -> dispatch Login ]
    [ R.text "Forgot password" ]
  , R.a
    [ RP.onClick \_ -> dispatch Login
    , RP.href $ state.facebookUrl
    ]
    [ R.text "Login with Facebook" ]
  , R.div
    [ RP.onClick \_ -> dispatch Login ]
    [ R.text "Register" ]
  ]

performAction :: T.PerformAction _ State _ Action
performAction = T.asyncOne' handler
  where
    handler :: Action -> _ -> State -> Aff _ (State -> State)
    handler NoOp _ state = do
      url <- sendSync state.socket (RPC.AuthFacebookUrl "" [])
      pure id
    handler Login _ state = do
      url <- sendSync state.socket (RPC.AuthFacebookUrl "" [])
      pure id
    handler (TextChanged lens v) _ state = do
      pure $ \s -> set lens v s

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main :: forall e. Eff (dom :: DOM.DOM, websocket :: S.WebSocket, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  socket <- S.connect "ws://localhost:8538" true
    { connected : \_ -> return unit
    , disconnected : return unit
    , message : \_ -> return unit
    }

  {-
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
  -}

  runAff throwException (const (pure unit)) $ do
    url <- sendSync socket (RPC.AuthFacebookUrl "" [])

    liftEff $ do
      let component = T.createClass spec (emptyState socket url)

      document <- DOM.window >>= DOM.document
      container <- fromJust <<< toMaybe <$> DOM.querySelector "#main" (DOM.htmlDocumentToParentNode document)

      R.render (R.createFactory component {}) container
      return unit
