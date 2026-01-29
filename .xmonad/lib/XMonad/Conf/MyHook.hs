{-# OPTIONS_GHC -Wno-deprecations #-}
module XMonad.Conf.MyHook where

import qualified Data.List as L
import Data.Monoid

import Graphics.X11.Xlib.Extras

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook)
import XMonad.Layout.Roledex
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import qualified XMonad.Layout.LayoutModifier as LM
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoFrillsDecoration

import XMonad.Actions.MetaMeta
import XMonad.Actions.TerminalAction
import XMonad.Actions.IntelliJTerminal
import XMonad.Actions.FloatAwareFocus
import XMonad.Actions.DrawShape
import XMonad.Layout.AndroidLikeWindowView
import XMonad.Layout.WindowViewableLayout
import XMonad.Layout.CompositeTall
import XMonad.Layout.MyMultiToggle

import XMonad.Util.DocksSupport
import XMonad.Util.VirtualMouse
import XMonad.Util.PhysicalScreen
import XMonad.Util.Performance
import XMonad.Util.ManageHookUtils
import XMonad.Util.SwitchableLogHook
import XMonad.Util.WorkspaceHistory
import XMonad.Util.AdvancedMouse
import XMonad.Util.VirtualScreen
import XMonad.Util.HandleScreenChange
import XMonad.Util.HandleEventHooks
import XMonad.Util.DunstSupport
import XMonad.Util.MyUtils

import XMonad.Conf.MyConf
import XMonad.Conf.MyTerminalAction

myStartupHook =
    startupHook baseConfig <+>
    docksStartupHook <+>
    rePhysicalScreen priorityDisplayEDIDs <+>
    initializeScreenMouses <+>
    grabMetaKey [xK_Super_L, xK_Super_R] <+>
    spawn "xmodmap ~/.xmodmap"


myManageHookAll = manageHook baseConfig -- defaultConfig
                       <+> docksManageHook
                       <+> myScratchpadsManageHook
                       <+> terminalManageHook myTerminal myTerminalActions
                       <+> ((fmap (L.isSuffixOf ".onBottom") appName) --> onBottom)
                       <+> (stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> onCenter' 0.1)
                       <+> ((isDialog <&&> (className =? "Gimp")) --> onCenter' 0.1)
                       <+> ((className =? "jetbrains-idea") <&&> (title =? "win0") --> doFloat)
                       <+> intelliJTerminalManageHook intelliJTerminalEnv
                       <+> ((appName =? "gnome-screenshot") --> doIgnore)
--                       <+> ((className =? "Vivaldi-stable") <&&> (stringProperty "WM_WINDOW_ROLE" =? "pop-up") --> onCenter' 0.3)
--                       <+> (ask >>= \w -> liftX (debugWindow w))

--myLayout = (ResizableTall 1 (3/100) (1/2) [])
myLayoutHookAll = avoidStruts $ WindowViewableLayout Normal (
                                      (noBorders $ AndroidLikeWindowView (1/7) (3/100) (1/30) (1/100))
                                  ||| (Roledex)) $
                       toggleLayouts (renamed [Replace "■"] $ noBorders Full) $
                       (   (renamed [Replace "┣"] $ mkToggleInitial (single TitleTransformer) TitleTransformer $ myLayout)
                       ||| (renamed [Replace "┳"] $ mkToggleInitial (single TitleTransformer) TitleTransformer $ Mirror myLayout)
--                       ||| (renamed [Replace "┳"] $ Mirror myLayout)
--                       ||| (Circle)
--                       ||| (OneBig (3/4) (3/4))
--                       ||| (Accordion)
                       )
-- myLayout = measureLayoutHook "myLayout" $ compositeTall (3/100) wide
myLayout = compositeTall (3/100) (toggleLayouts (Full) wide)
  where wide = simpleWide (3/100)
myLayoutForVirtualScreen = compositeTall (3/100) (toggleLayouts (Full) wide)
  where wide = simpleWide (3/100)

data TitleTransformer = TitleTransformer deriving (Read, Show, Eq, Typeable)
instance Transformer TitleTransformer Window where
    transform TitleTransformer x k = k (noFrillsDeco shrinkText mySDConfig x) (\(LM.ModifiedLayout _ x') -> x')

myLogHook xmobarLogHook = switchableLogHook $ do
    measure "virtualScreenLogHook" virtualScreenLogHook
    measure "xmobarLogHook" $ xmobarLogHook
    measure "checkAndHandleDisplayChange" $ handleScreenChange moveScreenMouseToLastPosition
    measure "floatOnUp" $ floatOnUp
    measure "terminalLogHook" $ terminalLogHook myTerminal myTerminalActions
    measure "workspaceHistoryLogHook" $ workspaceHistoryLogHook 10

myHandleEventHook =
    measureEventHook "advancedMouseEventHook" advancedMouseEventHook <+>
   -- (\e ->
       -- case e of
         -- (ClientMessageEvent {ev_message_type = mt, ev_data = d, ev_window = w}) -> do
              -- names <- withDisplay $ \d -> io $ getAtomNames d [mt]
              -- if (not $ L.null names) && (head names == "_NET_ACTIVE_WINDOW") then do
                -- spawn $ "echo 'Ignore " ++ (show e) ++ "," ++ (show names) ++ "' >> /tmp/xmonad.debug.event"
                  -- withDisplay $ \dpy -> withWindowAttributes dpy w $ \wa -> io $ allocaXEvent $ \ev -> do
                    -- setEventType ev configureNotify
                    -- setConfigureEvent ev w w
                        -- (wa_x wa) (wa_y wa) (wa_width wa)
                        -- (wa_height wa) (wa_border_width wa) none (wa_override_redirect wa)
                    -- sendEvent dpy w False 0 ev
                -- return (All False)
              -- else
                -- return (All True)
         -- (AnyEvent {}) -> do
                -- spawn $ "echo 'Ignore " ++ (show e) ++ "' >> /tmp/xmonad.debug.event"
                -- return (All False)
         -- (PropertyEvent {}) -> do
                -- spawn $ "echo 'Ignore " ++ (show e) ++ "' >> /tmp/xmonad.debug.event"
                -- return (All False)
         -- _ -> do
              -- return (All True)) <+>
--    measureEventHook "handleEventHook-gnomeConfig" (handleEventHook gnomeConfig) <+>
    (\e ->
         case e of
           (ClientMessageEvent{ev_window = w, ev_message_type = mt, ev_data = d}) -> do
             withWindowSet $ \s -> do
               a_aw <- getAtom "_NET_ACTIVE_WINDOW"
               if mt == a_aw && (head d) /= 2 && W.peek s /= Just w then do
                   smartGreedyViewWindow w
               else return ()
             return (All True)
           _ -> return (All True)) <+>
    measureEventHook "docksEventHook" docksEventHook <+>
    measureEventHook "loggingCurrentScreenMousePositionEventHook" loggingCurrentScreenMousePositionEventHook <+>
    measureEventHook "myScratchpadsHandleEventHook" myScratchpadsHandleEventHook <+>
    measureEventHook "myTerminalActionHandleEventHook" myTerminalActionHandleEventHook <+>
    measureEventHook "eventhook1" (\e ->
      case e of
        (ConfigureRequestEvent ev_event_type ev_serial ev_send_event ev_event_display ev_parent ev_window ev_x ev_y ev_width ev_height ev_border_width ev_above ev_detail ev_value_mask) -> do
--             n <- runQuery className ev_window
--             spawn $ "echo '" ++ n ++ ":" ++ (show e) ++ "' >> /tmp/xmonad.debug.event"
--             withWindowSet $ \ws -> do
--               let pairs = M.assocs $ W.floating ws
--               forM pairs $ \(win, rect) -> do
--                 n <- runQuery title win
--                 spawn $ "echo '" ++ n ++ ":" ++ (show rect) ++ "' >> /tmp/xmonad.debug.event"
--             spawn $ "echo '' >> /tmp/xmonad.debug.event"

-- Performance
--             ifX (testBit ev_value_mask 6) $ windows (\s -> W.focusWindow ev_window s)
             return (All True)
        _ -> return (All True)) <+>
    --(\e -> do
       -- case e of
         -- (PropertyEvent ev_event_type ev_serial ev_send_event ev_event_display ev_window ev_atom ev_time ev_propstate) -> do
              -- withWindowSet (\ws -> spawn $ "echo '" ++ (show $ W.current ws) ++ "' >> /tmp/xmonad.debug.event")
              -- names <- withDisplay $ \d -> io $ getAtomNames d [ev_atom]
              -- spawn $ "echo '" ++ (show e) ++ "," ++ (show names) ++ "' >> /tmp/xmonad.debug.event"
              -- return (All True)
         -- (ClientMessageEvent {ev_message_type = mt, ev_data = d, ev_window = w}) -> do
              -- withWindowSet (\ws -> spawn $ "echo '" ++ (show $ W.current ws) ++ "' >> /tmp/xmonad.debug.event")
              -- names <- withDisplay $ \d -> io $ getAtomNames d [mt]
              -- if (not $ L.null names) && (head names == "_NET_WM_STATE") then do
                -- ns <- withDisplay $ \dpy -> io $ getAtomNames dpy [fromIntegral $ d!!1]
                -- spawn $ "echo '" ++ (show e) ++ "," ++ (show names) ++ "," ++ (show ns) ++ "' >> /tmp/xmonad.debug.event"
                -- if (not $ L.null ns) && (head ns == "_NET_WM_STATE_FULLSCREEN") then
                  -- withDisplay $ \dpy -> withWindowAttributes dpy w $ \wa -> io $ allocaXEvent $ \ev -> do
                    -- setEventType ev configureNotify
                    -- setConfigureEvent ev w w
                        -- (wa_x wa) (wa_y wa) (wa_width wa)
                        -- (wa_height wa) (wa_border_width wa) none (wa_override_redirect wa)
                    -- sendEvent dpy w False 0 ev
                -- else return ()
              -- else
                -- spawn $ "echo '" ++ (show e) ++ "," ++ (show names) ++ "' >> /tmp/xmonad.debug.event"
              -- return (All True)
--          _ -> do
--               withWindowSet (\ws -> spawn $ "echo '" ++ (show $ W.current ws) ++ "' >> /tmp/xmonad.debug.event")
--               spawn $ "echo '" ++ (show e) ++ "' >> /tmp/xmonad.debug.event"
--               return (All True)) <+>
    measureEventHook "keepWindowSizeHandleEventHook-file" (keepWindowSizeHandleEventHook $ stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog") <+>
    measureEventHook "keepWindowSizeHandleEventHook-gimp" (keepWindowSizeHandleEventHook $ (isDialog <&&> (className =? "Gimp"))) <+>
    measureEventHook "fullScreenEventHook" fullScreenEventHook <+>
    measureEventHook "handleMetaMeta" (handleMetaMeta [xK_Super_L, xK_Super_R] 300 (myNamedScratchpadAction "fzf_actions")) <+>
    measureEventHook "dunstEventHook" dunstEventHook <+>
    measureEventHook "redrawAllShapes" drawShapeEventHook <+>
    measureEventHook "virtualScreenEventHandler" (virtualScreenEventHandler purpleRGB darkBlueRGB)
