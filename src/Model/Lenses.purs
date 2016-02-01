module Model.Lenses where

import Prelude (Unit, unit, const)
import Data.Lens (Lens, PrismP, lens, prism)
import Data.Either (Either(..))
import Model


_NoOp :: PrismP Action Unit
_NoOp = prism (const NoOp) unwrap
  where
  unwrap NoOp = Right unit
  unwrap y = Left y

_Login :: PrismP Action Unit
_Login = prism (const Login) unwrap
  where
  unwrap Login = Right unit
  unwrap y = Left y

_LoginScreen :: PrismP Screen Unit
_LoginScreen = prism (const LoginScreen) unwrap
  where
  unwrap LoginScreen = Right unit
  unwrap y = Left y

_RegisterScreen :: PrismP Screen Unit
_RegisterScreen = prism (const RegisterScreen) unwrap
  where
  unwrap RegisterScreen = Right unit
  unwrap y = Left y

_ResetPasswordScreen :: PrismP Screen Unit
_ResetPasswordScreen = prism (const ResetPasswordScreen) unwrap
  where
  unwrap ResetPasswordScreen = Right unit
  unwrap y = Left y

regName :: forall a b r. Lens { "regName" :: a | r } { "regName" :: b | r } a b
regName = lens _."regName" (_ { "regName" = _ })

regEmail :: forall a b r. Lens { "regEmail" :: a | r } { "regEmail" :: b | r } a b
regEmail = lens _."regEmail" (_ { "regEmail" = _ })

regPassword :: forall a b r. Lens { "regPassword" :: a | r } { "regPassword" :: b | r } a b
regPassword = lens _."regPassword" (_ { "regPassword" = _ })

regRepeatPassword :: forall a b r. Lens { "regRepeatPassword" :: a | r } { "regRepeatPassword" :: b | r } a b
regRepeatPassword = lens _."regRepeatPassword" (_ { "regRepeatPassword" = _ })

loginName :: forall a b r. Lens { "loginName" :: a | r } { "loginName" :: b | r } a b
loginName = lens _."loginName" (_ { "loginName" = _ })

loginPassword :: forall a b r. Lens { "loginPassword" :: a | r } { "loginPassword" :: b | r } a b
loginPassword = lens _."loginPassword" (_ { "loginPassword" = _ })

resetEmail :: forall a b r. Lens { "resetEmail" :: a | r } { "resetEmail" :: b | r } a b
resetEmail = lens _."resetEmail" (_ { "resetEmail" = _ })

session :: forall a b r. Lens { "session" :: a | r } { "session" :: b | r } a b
session = lens _."session" (_ { "session" = _ })

socket :: forall a b r. Lens { "socket" :: a | r } { "socket" :: b | r } a b
socket = lens _."socket" (_ { "socket" = _ })

facebookUrl :: forall a b r. Lens { "facebookUrl" :: a | r } { "facebookUrl" :: b | r } a b
facebookUrl = lens _."facebookUrl" (_ { "facebookUrl" = _ })

screen :: forall a b r. Lens { "screen" :: a | r } { "screen" :: b | r } a b
screen = lens _."screen" (_ { "screen" = _ })

regState :: forall a b r. Lens { "regState" :: a | r } { "regState" :: b | r } a b
regState = lens _."regState" (_ { "regState" = _ })

loginState :: forall a b r. Lens { "loginState" :: a | r } { "loginState" :: b | r } a b
loginState = lens _."loginState" (_ { "loginState" = _ })

resetPasswordState :: forall a b r. Lens { "resetPasswordState" :: a | r } { "resetPasswordState" :: b | r } a b
resetPasswordState = lens _."resetPasswordState" (_ { "resetPasswordState" = _ })
