{-# LANGUAGE FlexibleContexts #-}

module XMonad.Actions.MetaMeta (
  grabMetaKey, handleMetaMeta
) where

import Control.Exception.Extensible as E
import Control.Monad (mfilter, foldM, filterM, mapM, forM, forever, mplus, msum, when)
import Data.Bits
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Monoid

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

grabMetaKey metaKeys = do
  XConf { display = dpy, theRoot = rootw } <- ask
  let grab kc m = io $ grabKey dpy kc m rootw True grabModeAsync grabModeAsync
      (minCode, maxCode) = displayKeycodes dpy
      allCodes = [fromIntegral minCode .. fromIntegral maxCode]
  syms <- forM allCodes $ \code -> io (keycodeToKeysym dpy code 0)
  let keysymMap = M.fromListWith (++) (zip syms [[code] | code <- allCodes])
      keysymToKeycodes sym = M.findWithDefault [] sym keysymMap
  forM metaKeys $ \mk ->
    forM (keysymToKeycodes mk) $ \kc ->
      grab kc anyModifier
  return ()

handleMetaMeta metaKeys interval x e@(KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code, ev_time = time}) =
  withDisplay $ \dpy -> do
    s <- io $ keycodeToKeysym dpy code 0
    if L.elem s metaKeys  then do
        RemapRequested r <- XS.get
        if r then do
            XS.put $ RemapRequested False
            grabMetaKey metaKeys
        else return ()
        if t == keyPress then do
            LastMetaPress lastMetaPress <- XS.get
            case lastMetaPress of
              Just (lastKey, lastTime) -> do
                let diffTime = time - lastTime
                if lastKey == s && diffTime < interval then x else return ()
              _ -> return ()
            XS.put $ LastMetaPress $ Just (s, time)
        else return ()
        return $ All False
    else do
        XS.put $ LastMetaPress $ Nothing
        return $ All True
handleMetaMeta _ _ _ e@(MappingNotifyEvent {}) = do
  XS.put $ RemapRequested True
  return (All True)
handleMetaMeta metaKeys _ _ e = do
  RemapRequested r <- XS.get
  if r then do
      XS.put $ RemapRequested False
      grabMetaKey metaKeys
  else return ()
  return (All True)

data LastMetaPress = LastMetaPress (Maybe (KeySym, Time)) deriving Typeable
instance ExtensionClass LastMetaPress where
  initialValue = LastMetaPress Nothing
data RemapRequested = RemapRequested Bool deriving Typeable
instance ExtensionClass RemapRequested where
  initialValue = RemapRequested False
