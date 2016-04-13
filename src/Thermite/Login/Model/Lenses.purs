module Thermite.Login.Model.Lenses where

import Prelude (Unit, unit, const)
import Data.Lens (Lens, PrismP, lens, prism)
import Data.Either (Either(..))
import Thermite.Login.Model


_Logout :: forall err userdata uid. PrismP (Action uid userdata err) Unit
_Logout = prism (const Logout) unwrap
  where
  unwrap Logout = Right unit
  unwrap y = Left y

_SubRegisterAction :: forall err userdata uid. PrismP (Action uid userdata err) (RegisterAction userdata err)
_SubRegisterAction = prism SubRegisterAction unwrap
  where
  unwrap (SubRegisterAction x) = Right x
  unwrap y = Left y

_SubLoginAction :: forall err userdata uid. PrismP (Action uid userdata err) LoginAction
_SubLoginAction = prism SubLoginAction unwrap
  where
  unwrap (SubLoginAction x) = Right x
  unwrap y = Left y

_SubResetAction :: forall err userdata uid. PrismP (Action uid userdata err) ResetAction
_SubResetAction = prism SubResetAction unwrap
  where
  unwrap (SubResetAction x) = Right x
  unwrap y = Left y

_SubSetNewPasswordAction :: forall err userdata uid. PrismP (Action uid userdata err) SetNewPasswordAction
_SubSetNewPasswordAction = prism SubSetNewPasswordAction unwrap
  where
  unwrap (SubSetNewPasswordAction x) = Right x
  unwrap y = Left y

_ChangeScreen :: forall err userdata uid. PrismP (Action uid userdata err) Screen
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

_SetNewPasswordScreen :: PrismP Screen String
_SetNewPasswordScreen = prism SetNewPasswordScreen unwrap
  where
  unwrap (SetNewPasswordScreen x) = Right x
  unwrap y = Left y

name :: forall a b r. Lens { "name" :: a | r } { "name" :: b | r } a b
name = lens _."name" (_ { "name" = _ })

password :: forall a b r. Lens { "password" :: a | r } { "password" :: b | r } a b
password = lens _."password" (_ { "password" = _ })

repeatPassword :: forall a b r. Lens { "repeatPassword" :: a | r } { "repeatPassword" :: b | r } a b
repeatPassword = lens _."repeatPassword" (_ { "repeatPassword" = _ })

resetPassword :: forall a b r. Lens { "resetPassword" :: a | r } { "resetPassword" :: b | r } a b
resetPassword = lens _."resetPassword" (_ { "resetPassword" = _ })

login :: forall a b r. Lens { "login" :: a | r } { "login" :: b | r } a b
login = lens _."login" (_ { "login" = _ })

email :: forall a b r. Lens { "email" :: a | r } { "email" :: b | r } a b
email = lens _."email" (_ { "email" = _ })

forgotPassword :: forall a b r. Lens { "forgotPassword" :: a | r } { "forgotPassword" :: b | r } a b
forgotPassword = lens _."forgotPassword" (_ { "forgotPassword" = _ })

register :: forall a b r. Lens { "register" :: a | r } { "register" :: b | r } a b
register = lens _."register" (_ { "register" = _ })

setPassword :: forall a b r. Lens { "setPassword" :: a | r } { "setPassword" :: b | r } a b
setPassword = lens _."setPassword" (_ { "setPassword" = _ })

loginWithFacebook :: forall a b r. Lens { "loginWithFacebook" :: a | r } { "loginWithFacebook" :: b | r } a b
loginWithFacebook = lens _."loginWithFacebook" (_ { "loginWithFacebook" = _ })

errUserOrPasswordIncorrect :: forall a b r. Lens { "errUserOrPasswordIncorrect" :: a | r } { "errUserOrPasswordIncorrect" :: b | r } a b
errUserOrPasswordIncorrect = lens _."errUserOrPasswordIncorrect" (_ { "errUserOrPasswordIncorrect" = _ })

errUserOrEmailAlreadyTaken :: forall a b r. Lens { "errUserOrEmailAlreadyTaken" :: a | r } { "errUserOrEmailAlreadyTaken" :: b | r } a b
errUserOrEmailAlreadyTaken = lens _."errUserOrEmailAlreadyTaken" (_ { "errUserOrEmailAlreadyTaken" = _ })

userCreatedSuccessfully :: forall a b r. Lens { "userCreatedSuccessfully" :: a | r } { "userCreatedSuccessfully" :: b | r } a b
userCreatedSuccessfully = lens _."userCreatedSuccessfully" (_ { "userCreatedSuccessfully" = _ })

passwordResetMailSentSuccessfully :: forall a b r. Lens { "passwordResetMailSentSuccessfully" :: a | r } { "passwordResetMailSentSuccessfully" :: b | r } a b
passwordResetMailSentSuccessfully = lens _."passwordResetMailSentSuccessfully" (_ { "passwordResetMailSentSuccessfully" = _ })

newPasswordSetSuccessfully :: forall a b r. Lens { "newPasswordSetSuccessfully" :: a | r } { "newPasswordSetSuccessfully" :: b | r } a b
newPasswordSetSuccessfully = lens _."newPasswordSetSuccessfully" (_ { "newPasswordSetSuccessfully" = _ })

custom :: forall a b r. Lens { "custom" :: a | r } { "custom" :: b | r } a b
custom = lens _."custom" (_ { "custom" = _ })

redirectUrl :: forall a b r. Lens { "redirectUrl" :: a | r } { "redirectUrl" :: b | r } a b
redirectUrl = lens _."redirectUrl" (_ { "redirectUrl" = _ })

redirectToScreen :: forall a b r. Lens { "redirectToScreen" :: a | r } { "redirectToScreen" :: b | r } a b
redirectToScreen = lens _."redirectToScreen" (_ { "redirectToScreen" = _ })

sendRequest :: forall a b r. Lens { "sendRequest" :: a | r } { "sendRequest" :: b | r } a b
sendRequest = lens _."sendRequest" (_ { "sendRequest" = _ })

sessionLength :: forall a b r. Lens { "sessionLength" :: a | r } { "sessionLength" :: b | r } a b
sessionLength = lens _."sessionLength" (_ { "sessionLength" = _ })

defaultUserData :: forall a b r. Lens { "defaultUserData" :: a | r } { "defaultUserData" :: b | r } a b
defaultUserData = lens _."defaultUserData" (_ { "defaultUserData" = _ })

registerMask :: forall a b r. Lens { "registerMask" :: a | r } { "registerMask" :: b | r } a b
registerMask = lens _."registerMask" (_ { "registerMask" = _ })

loginMask :: forall a b r. Lens { "loginMask" :: a | r } { "loginMask" :: b | r } a b
loginMask = lens _."loginMask" (_ { "loginMask" = _ })

resetPasswordMask :: forall a b r. Lens { "resetPasswordMask" :: a | r } { "resetPasswordMask" :: b | r } a b
resetPasswordMask = lens _."resetPasswordMask" (_ { "resetPasswordMask" = _ })

setNewPasswordMask :: forall a b r. Lens { "setNewPasswordMask" :: a | r } { "setNewPasswordMask" :: b | r } a b
setNewPasswordMask = lens _."setNewPasswordMask" (_ { "setNewPasswordMask" = _ })

_RegisterTextChanged :: forall err userdata. PrismP (RegisterAction userdata err) (RegisterState userdata err -> RegisterState userdata err)
_RegisterTextChanged = prism RegisterTextChanged unwrap
  where
  unwrap (RegisterTextChanged x) = Right x
  unwrap y = Left y

_Register :: forall err userdata. PrismP (RegisterAction userdata err) Unit
_Register = prism (const Register) unwrap
  where
  unwrap Register = Right unit
  unwrap y = Left y

regName :: forall a b r. Lens { "regName" :: a | r } { "regName" :: b | r } a b
regName = lens _."regName" (_ { "regName" = _ })

regEmail :: forall a b r. Lens { "regEmail" :: a | r } { "regEmail" :: b | r } a b
regEmail = lens _."regEmail" (_ { "regEmail" = _ })

regPassword :: forall a b r. Lens { "regPassword" :: a | r } { "regPassword" :: b | r } a b
regPassword = lens _."regPassword" (_ { "regPassword" = _ })

regRepeatPassword :: forall a b r. Lens { "regRepeatPassword" :: a | r } { "regRepeatPassword" :: b | r } a b
regRepeatPassword = lens _."regRepeatPassword" (_ { "regRepeatPassword" = _ })

regLoading :: forall a b r. Lens { "regLoading" :: a | r } { "regLoading" :: b | r } a b
regLoading = lens _."regLoading" (_ { "regLoading" = _ })

regUserData :: forall a b r. Lens { "regUserData" :: a | r } { "regUserData" :: b | r } a b
regUserData = lens _."regUserData" (_ { "regUserData" = _ })

regResult :: forall a b r. Lens { "regResult" :: a | r } { "regResult" :: b | r } a b
regResult = lens _."regResult" (_ { "regResult" = _ })

_LoginTextChanged :: PrismP LoginAction (LoginState -> LoginState)
_LoginTextChanged = prism LoginTextChanged unwrap
  where
  unwrap (LoginTextChanged x) = Right x
  unwrap y = Left y

_Login :: PrismP LoginAction Unit
_Login = prism (const Login) unwrap
  where
  unwrap Login = Right unit
  unwrap y = Left y

_LoginWithFacebook :: PrismP LoginAction Unit
_LoginWithFacebook = prism (const LoginWithFacebook) unwrap
  where
  unwrap LoginWithFacebook = Right unit
  unwrap y = Left y

_LoginScreenChanged :: PrismP LoginAction Screen
_LoginScreenChanged = prism LoginScreenChanged unwrap
  where
  unwrap (LoginScreenChanged x) = Right x
  unwrap y = Left y

loginName :: forall a b r. Lens { "loginName" :: a | r } { "loginName" :: b | r } a b
loginName = lens _."loginName" (_ { "loginName" = _ })

loginPassword :: forall a b r. Lens { "loginPassword" :: a | r } { "loginPassword" :: b | r } a b
loginPassword = lens _."loginPassword" (_ { "loginPassword" = _ })

loginError :: forall a b r. Lens { "loginError" :: a | r } { "loginError" :: b | r } a b
loginError = lens _."loginError" (_ { "loginError" = _ })

loginLoading :: forall a b r. Lens { "loginLoading" :: a | r } { "loginLoading" :: b | r } a b
loginLoading = lens _."loginLoading" (_ { "loginLoading" = _ })

_ResetTextChanged :: PrismP ResetAction (ResetPasswordState -> ResetPasswordState)
_ResetTextChanged = prism ResetTextChanged unwrap
  where
  unwrap (ResetTextChanged x) = Right x
  unwrap y = Left y

_ResetPassword :: PrismP ResetAction Unit
_ResetPassword = prism (const ResetPassword) unwrap
  where
  unwrap ResetPassword = Right unit
  unwrap y = Left y

locale :: forall a b r. Lens { "locale" :: a | r } { "locale" :: b | r } a b
locale = lens _."locale" (_ { "locale" = _ })

resetEmail :: forall a b r. Lens { "resetEmail" :: a | r } { "resetEmail" :: b | r } a b
resetEmail = lens _."resetEmail" (_ { "resetEmail" = _ })

resetLoading :: forall a b r. Lens { "resetLoading" :: a | r } { "resetLoading" :: b | r } a b
resetLoading = lens _."resetLoading" (_ { "resetLoading" = _ })

resetShowSuccessMessage :: forall a b r. Lens { "resetShowSuccessMessage" :: a | r } { "resetShowSuccessMessage" :: b | r } a b
resetShowSuccessMessage = lens _."resetShowSuccessMessage" (_ { "resetShowSuccessMessage" = _ })

_SetNewPasswordTextChanged :: PrismP SetNewPasswordAction (SetNewPasswordState -> SetNewPasswordState)
_SetNewPasswordTextChanged = prism SetNewPasswordTextChanged unwrap
  where
  unwrap (SetNewPasswordTextChanged x) = Right x
  unwrap y = Left y

_SetNewPassword :: PrismP SetNewPasswordAction String
_SetNewPassword = prism SetNewPassword unwrap
  where
  unwrap (SetNewPassword x) = Right x
  unwrap y = Left y

token :: forall a b r. Lens { "token" :: a | r } { "token" :: b | r } a b
token = lens _."token" (_ { "token" = _ })

setpwdPassword :: forall a b r. Lens { "setpwdPassword" :: a | r } { "setpwdPassword" :: b | r } a b
setpwdPassword = lens _."setpwdPassword" (_ { "setpwdPassword" = _ })

setpwdRepeatPassword :: forall a b r. Lens { "setpwdRepeatPassword" :: a | r } { "setpwdRepeatPassword" :: b | r } a b
setpwdRepeatPassword = lens _."setpwdRepeatPassword" (_ { "setpwdRepeatPassword" = _ })

setpwdShowSuccessMessage :: forall a b r. Lens { "setpwdShowSuccessMessage" :: a | r } { "setpwdShowSuccessMessage" :: b | r } a b
setpwdShowSuccessMessage = lens _."setpwdShowSuccessMessage" (_ { "setpwdShowSuccessMessage" = _ })

setpwdLoading :: forall a b r. Lens { "setpwdLoading" :: a | r } { "setpwdLoading" :: b | r } a b
setpwdLoading = lens _."setpwdLoading" (_ { "setpwdLoading" = _ })

sessionId :: forall a b r. Lens { "sessionId" :: a | r } { "sessionId" :: b | r } a b
sessionId = lens _."sessionId" (_ { "sessionId" = _ })

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

setNewPasswordState :: forall a b r. Lens { "setNewPasswordState" :: a | r } { "setNewPasswordState" :: b | r } a b
setNewPasswordState = lens _."setNewPasswordState" (_ { "setNewPasswordState" = _ })
