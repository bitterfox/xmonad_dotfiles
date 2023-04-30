
module XMonad.Util.VirtualMouse (
  loggingCurrentScreenMousePositionEventHook,
  logCurrentScreenMousePosition,
  queryPointerAndLogCurrentScreenMousePosition,
  moveScreenMouseToLastPosition,
  getVirtualMousePosition,
  setVirtualMousePosition,
  changeCurrentVirtualMouse
) where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Control.Monad (mfilter)

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.MyUtils

------------------------------------------------------------------------------------------
-- MousePosition
------------------------------------------------------------------------------------------
type MouseId = [Char]

data MousePositionMap = MousePositionMap (M.Map MouseId (Position, Position)) deriving Typeable
instance ExtensionClass MousePositionMap where
  initialValue = MousePositionMap M.empty

data MousePosition = MousePosition (Maybe (Position, Position)) deriving Typeable
instance ExtensionClass MousePosition where
  initialValue = MousePosition Nothing

data CurrentVirtualMouse = CurrentVirtualMouse (Maybe MouseId) deriving Typeable
instance ExtensionClass CurrentVirtualMouse where
  initialValue = CurrentVirtualMouse Nothing

screenIdToMouseId = show

loggingCurrentScreenMousePositionEventHook e = do
  logCurrentScreenMousePosition False False
  return (All True)

logCurrentScreenMousePosition :: Bool -> Bool -> X ()
logCurrentScreenMousePosition force fallback = withWindowSet $ \ws -> do
  MousePosition maybeLastMousePosition <- XS.get
  xconf <- ask
  let maybeMousePos = mousePosition xconf
  if maybeMousePos == Nothing && fallback then
      queryPointerAndLogCurrentScreenMousePosition force
  else ifX (maybeLastMousePosition /= maybeMousePos) $
    logCurrentScreenMousePosition' force ws maybeMousePos

queryPointerAndLogCurrentScreenMousePosition force = withWindowSet $ \ws -> do
  MousePosition maybeLastMousePosition <- XS.get
  xconf <- ask
  (ok, _, _, x, y, _, _, _) <- io $ queryPointer (display xconf) (theRoot xconf)
  ifX ok $
    logCurrentScreenMousePosition' force ws $ Just (fromIntegral x, fromIntegral y)

logCurrentScreenMousePosition' force ws maybeMousePos =
    whenJust maybeMousePos $ \(x, y) -> do
      maybeScreen <- pointScreen x y
      if force then whenJust (W.screen <$> maybeScreen) $ \s -> do
        setVirtualMousePosition (screenIdToMouseId s) (x, y)
        XS.put $ MousePosition $ maybeMousePos
      else whenJust (mfilter ((W.screen $ W.current ws) ==) $ fmap W.screen maybeScreen) $ \s -> do
        setVirtualMousePosition (screenIdToMouseId s) (x, y)
        XS.put $ MousePosition $ maybeMousePos

moveScreenMouseToLastPosition :: X ()
moveScreenMouseToLastPosition =
  whenX (isNothing <$> dragging <$> get) $
    withWindowSet $ \ws -> do
      let rect = screenRect $ W.screenDetail $ W.current ws
      maybeMousePos <- mousePosition <$> ask
      case maybeMousePos of
        Just (x, y) -> ifX (not $ pointWithin x y rect) $ moveScreenMouseToLastPosition' ws
        _ -> moveScreenMouseToLastPosition' ws
moveScreenMouseToLastPosition' ws = do
  let s = W.current ws
  let mid = screenIdToMouseId $ W.screen s
  logCurrentScreenMousePosition True True
  changeCurrentVirtualMouse mid $ centerOfScreen s

centerOfScreen s = do
  let rect = screenRect $ W.screenDetail s
  let x = truncate $ (fromIntegral $ rect_x rect) + (fromIntegral $ rect_width rect) / 2
  let y = truncate $ (fromIntegral $ rect_y rect) + (fromIntegral $ rect_height rect) / 2
  (x, y)

getVirtualMousePosition :: MouseId -> X (Maybe (Position, Position))
getVirtualMousePosition mid = do
  MousePositionMap lastMousePositions <- XS.get
  return $ M.lookup mid lastMousePositions

setVirtualMousePosition :: MouseId -> (Position, Position) -> X ()
setVirtualMousePosition mid p = do
  MousePositionMap lastMousePositions <- XS.get
  XS.put $ MousePositionMap $ M.insert mid p lastMousePositions

changeCurrentVirtualMouse :: MouseId -> (Position, Position) -> X ()
changeCurrentVirtualMouse mid (defX, defY) = do
  p <- getVirtualMousePosition mid
  let (x, y) = fromMaybe (defX, defY) p
  moveMouseTo x y
  XS.put $ CurrentVirtualMouse $ Just mid

moveMouseTo x y = do
  rootw <- asks theRoot
  withDisplay $ \d ->
    io $ warpPointer d none rootw 0 0 0 0 (fromIntegral x) (fromIntegral y)
------------------------------------------------------------------------------------------
-- MousePosition
------------------------------------------------------------------------------------------
