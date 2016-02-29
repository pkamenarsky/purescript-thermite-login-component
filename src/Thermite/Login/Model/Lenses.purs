module Thermite.Login.Model.Lenses where

import Prelude (Unit, unit, const)
import Data.Lens (Lens, PrismP, lens, prism)
import Data.Either (Either(..))
import Thermite.Login.Model


_Login :: forall userdata uid. PrismP (Action uid userdata) Unit
_Login = prism (const Login) unwrap
  where
  unwrap Login = Right unit
  unwrap y = Left y

_LoginWithFacebook :: forall userdata uid. PrismP (Action uid userdata) Unit
_LoginWithFacebook = prism (const LoginWithFacebook) unwrap
  where
  unwrap LoginWithFacebook = Right unit
  unwrap y = Left y

_Register :: forall userdata uid. PrismP (Action uid userdata) Unit
_Register = prism (const Register) unwrap
  where
  unwrap Register = Right unit
  unwrap y = Left y

_ResetPassword :: forall userdata uid. PrismP (Action uid userdata) Unit
_ResetPassword = prism (const ResetPassword) unwrap
  where
  unwrap ResetPassword = Right unit
  unwrap y = Left y

_ChangeScreen :: forall userdata uid. PrismP (Action uid userdata) Screen
_ChangeScreen = prism ChangeScreen unwrap
  where
  unwrap (ChangeScreen x) = Right x
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

redirectUrl :: forall a b r. Lens { "redirectUrl" :: a | r } { "redirectUrl" :: b | r } a b
redirectUrl = lens _."redirectUrl" (_ { "redirectUrl" = _ })

socket :: forall a b r. Lens { "socket" :: a | r } { "socket" :: b | r } a b
socket = lens _."socket" (_ { "socket" = _ })

sessionLength :: forall a b r. Lens { "sessionLength" :: a | r } { "sessionLength" :: b | r } a b
sessionLength = lens _."sessionLength" (_ { "sessionLength" = _ })

defaultUserData :: forall a b r. Lens { "defaultUserData" :: a | r } { "defaultUserData" :: b | r } a b
defaultUserData = lens _."defaultUserData" (_ { "defaultUserData" = _ })

regName :: forall a b r. Lens { "regName" :: a | r } { "regName" :: b | r } a b
regName = lens _."regName" (_ { "regName" = _ })

regEmail :: forall a b r. Lens { "regEmail" :: a | r } { "regEmail" :: b | r } a b
regEmail = lens _."regEmail" (_ { "regEmail" = _ })

regPassword :: forall a b r. Lens { "regPassword" :: a | r } { "regPassword" :: b | r } a b
regPassword = lens _."regPassword" (_ { "regPassword" = _ })

regRepeatPassword :: forall a b r. Lens { "regRepeatPassword" :: a | r } { "regRepeatPassword" :: b | r } a b
regRepeatPassword = lens _."regRepeatPassword" (_ { "regRepeatPassword" = _ })

regResult :: forall a b r. Lens { "regResult" :: a | r } { "regResult" :: b | r } a b
regResult = lens _."regResult" (_ { "regResult" = _ })

loginName :: forall a b r. Lens { "loginName" :: a | r } { "loginName" :: b | r } a b
loginName = lens _."loginName" (_ { "loginName" = _ })

loginPassword :: forall a b r. Lens { "loginPassword" :: a | r } { "loginPassword" :: b | r } a b
loginPassword = lens _."loginPassword" (_ { "loginPassword" = _ })

loginError :: forall a b r. Lens { "loginError" :: a | r } { "loginError" :: b | r } a b
loginError = lens _."loginError" (_ { "loginError" = _ })

resetEmail :: forall a b r. Lens { "resetEmail" :: a | r } { "resetEmail" :: b | r } a b
resetEmail = lens _."resetEmail" (_ { "resetEmail" = _ })

sessionId :: forall a b r. Lens { "sessionId" :: a | r } { "sessionId" :: b | r } a b
sessionId = lens _."sessionId" (_ { "sessionId" = _ })

userId :: forall a b r. Lens { "userId" :: a | r } { "userId" :: b | r } a b
userId = lens _."userId" (_ { "userId" = _ })

sessionUser :: forall a b r. Lens { "sessionUser" :: a | r } { "sessionUser" :: b | r } a b
sessionUser = lens _."sessionUser" (_ { "sessionUser" = _ })

redirectingAfterLogin :: forall a b r. Lens { "redirectingAfterLogin" :: a | r } { "redirectingAfterLogin" :: b | r } a b
redirectingAfterLogin = lens _."redirectingAfterLogin" (_ { "redirectingAfterLogin" = _ })

screen :: forall a b r. Lens { "screen" :: a | r } { "screen" :: b | r } a b
screen = lens _."screen" (_ { "screen" = _ })

regState :: forall a b r. Lens { "regState" :: a | r } { "regState" :: b | r } a b
regState = lens _."regState" (_ { "regState" = _ })

loginState :: forall a b r. Lens { "loginState" :: a | r } { "loginState" :: b | r } a b
loginState = lens _."loginState" (_ { "loginState" = _ })

resetPasswordState :: forall a b r. Lens { "resetPasswordState" :: a | r } { "resetPasswordState" :: b | r } a b
resetPasswordState = lens _."resetPasswordState" (_ { "resetPasswordState" = _ })
