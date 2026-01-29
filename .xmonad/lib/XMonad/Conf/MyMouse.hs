module XMonad.Conf.MyMouse where

import qualified Data.List as L
import Data.Time.Clock

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Actions.MouseResize as MR
import XMonad.Util.EZConfig
import XMonad.Layout.ToggleLayouts

import XMonad.Util.AdvancedMouse
import XMonad.Layout.WindowViewableLayout
import XMonad.Actions.FloatAwareFocus
import XMonad.Util.VirtualScreen
import XMonad.Util.WorkspaceFamily

import XMonad.Util.MyUtils

configureMouse config =
  config {
    rootMask = (rootMask config) .|. buttonReleaseMask
  } `additionalMouseBindings` [
    -- left click
    ((mod4Mask, button1), \w -> focus w >> mouseMoveWindow w)
  , ((mod4Mask .|. controlMask, button1), \w -> focus w >> MR.mouseResizeWindow w
                                                        >> windows W.shiftMaster)
    -- middle click
  , ((mod4Mask, button3), \w -> do
      ws <- gets windowset
      ifX (isFloat ws w) $ do
          before <- gets windowset
          windows $ W.modify' $ \stack@(W.Stack t ls rs) ->
              if t == w then
                case L.filter (isFloat ws) $ ls ++ rs of
                  (nw:_) -> W.Stack nw (L.delete nw ls) $ (L.delete nw rs) ++ [w]
                  _ -> stack
              else W.Stack t (L.delete w ls) $ (L.delete w rs) ++ [w])
    -- wh left
  , ((0, 8), \w -> do
       windowViewState <- XS.get
       case windowViewState of
         Normal -> prevWS'
         WindowView -> windows floatAvoidFocusUp)
    -- wh right
  , ((0, 9), \w -> do
       windowViewState <- XS.get
       case windowViewState of
         Normal -> nextWS'
         WindowView -> windows floatAvoidFocusDown)
    -- left-hand-side first (G5)
  , ((0, 10), \w -> whenDoubleClick (0.300 :: NominalDiffTime) 10 10 nextVirtualScreen)
    -- GHub
  , ((0, 14), \w -> do
       focus w
       quitWindowView $ sendMessage ToggleLayout)
    -- left-side second (G4)
  , ((0, 11), \w -> spawn "echo G4 >> /mnt/hgfs/data/mission-control")
  , ((0, 15), \w -> return ())
    -- left click first (G8)
  , ((0, 12), \w -> spawn $ "xdotool key XF86Copy")
    -- left click first (G7)
  , ((0, 13), \w -> spawn $ "xdotool key XF86Paste")
  ]
  `advancedMouseBindings` [
    -- Hold left-hand-side first (G5)
       -- wh left
    ([10], (0, 8), \w -> prevVirtualScreen)
       -- wh right
  , ([10], (0, 9), \w -> nextVirtualScreen)
       -- left click
  , ([10], (0, 1), \w -> focus w >> kill)
       -- wh up
  , ([10], (0, 4), \w -> windows floatAvoidFocusUp)
       -- wh down
  , ([10], (0, 5), \w -> windows floatAvoidFocusDown)
  ] `ungrabButtons'`
  -- left click, middle click, right click, wh up, wh down
  [1, 2, 3, 4, 5]
