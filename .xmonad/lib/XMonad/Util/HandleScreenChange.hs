
module XMonad.Util.HandleScreenChange (
  handleScreenChange
) where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.MyUtils

data LastScreenId = LastScreenId (Maybe ScreenId) deriving Typeable
instance ExtensionClass LastScreenId where
  initialValue = LastScreenId Nothing

handleScreenChange action =
  withWindowSet $ \s -> do
    LastScreenId maybeScreenId <- XS.get
    case maybeScreenId of
      Just screenId ->
        ifX ((W.screen $ W.current s) /= screenId) $ do
          XS.put $ LastScreenId $ Just $ W.screen $ W.current s
          action
      Nothing -> do
          XS.put $ LastScreenId $ Just $ W.screen $ W.current s
          action
