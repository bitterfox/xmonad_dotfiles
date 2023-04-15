{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module XMonad.Util.Performance (
  measure,
  measureLayoutHook,
  measureEventHook,
  getDurations,
--  resetDurations
) where

import System.Clock
import qualified Data.Map.Strict as M
import Data.Maybe

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

data Durations = Durations (M.Map String (Integer, Integer)) deriving Typeable
instance ExtensionClass Durations where
  initialValue = Durations M.empty

measure name x = x
--measure name x = do
--  start <- io $ getMonotonicTimeNSec
--  a <- x
--  end <- io $ getMonotonicTimeNSec
--  let duration = end - start
--  Durations map <- XS.get
--  XS.put $ Durations $ M.alter (increment duration) name map
--  return a

increment d m = do
    let time = fst $ fromMaybe (0, 0) m
    let count = snd $ fromMaybe (0, 0) m
    Just (time + d, count + 1)

measureLayoutHook name l = PerformanceMonitoringLayout name l

measureEventHook name f = \e -> measure name $ f e

data PerformanceMonitoringLayout layout a = PerformanceMonitoringLayout String (layout a) deriving ( Read, Show )

instance (LayoutClass l a) => LayoutClass (PerformanceMonitoringLayout l) a where
    runLayout (W.Workspace i (PerformanceMonitoringLayout name l) ms) r = do
      (ws, ml) <- measure name $ runLayout (W.Workspace i l ms) r
      case ml of
        Just nl -> return (ws, Just $ PerformanceMonitoringLayout name nl)
        Nothing -> return (ws, Nothing)

    handleMessage (PerformanceMonitoringLayout name l) mess = do
      ml <- measure name $ handleMessage l mess
      return $ PerformanceMonitoringLayout name <$> ml

    description (PerformanceMonitoringLayout name l) = description l

-- resetDurations = XS.put $ Durations $ M.empty
getDurations :: X (M.Map String (Integer, Integer))
getDurations = do
 Durations map <- XS.get
 return map

getMonotonicTimeNSec = toNanoSecs <$> getTime Monotonic
