
module XMonad.Util.SwitchableLogHook (
  switchableLogHook,
  enableLogHook,
  disableLogHook,
  withoutLogHook
) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

------------------------------------------------------------------------------------------
-- SwitchableLogHook
------------------------------------------------------------------------------------------

data LogHookSwitch = LogHookSwitch Bool deriving (Typeable, Show)
instance ExtensionClass LogHookSwitch where
  initialValue = LogHookSwitch True

switchableLogHook logHook = do
  LogHookSwitch flag <- XS.get
  if flag then logHook else return ()

enableLogHook :: X ()
enableLogHook = do
  XS.put $ LogHookSwitch True
disableLogHook :: X ()
disableLogHook = do
  XS.put $ LogHookSwitch False

withoutLogHook x = do
  disableLogHook
  r <- x
  enableLogHook
  return r

------------------------------------------------------------------------------------------
-- SwitchableLogHook
------------------------------------------------------------------------------------------
