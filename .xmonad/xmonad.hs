{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
import System.Directory
import System.Exit
import System.IO
import System.IO.Error hiding (catch)
import System.Process (runInteractiveProcess, readProcess)

import Data.Bits
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as T

import Control.Concurrent
import Control.Exception.Extensible as E
import Control.Monad (mfilter, foldM, filterM, mapM, forM, forever, mplus, msum, when)

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Show as TS

import Codec.Binary.UTF8.String

import Foreign
import Foreign.C.Types

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.ManageHook

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.EvacuationLikeMac
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.IntelliJTerminal
import XMonad.Actions.Volume
import XMonad.Actions.GridSelect
import qualified XMonad.Actions.MouseResize as MR
import XMonad.Actions.Search (selectSearchBrowser, google)
import XMonad.Actions.Submap
import XMonad.Actions.TerminalAction
import XMonad.Actions.TerminalAction.GnomeBackend
import XMonad.Actions.WindowGo
--import XMonad.Config.Desktop (desktopLayoutModifiers)

import XMonad.Config.Desktop
import XMonad.Config.Gnome

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import qualified XMonad.Layout.LayoutModifier as LM
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Mosaic
import XMonad.Layout.MultiColumns
import XMonad.Layout.MyMultiToggle
import XMonad.Layout.MultiToggle.Instances
import qualified XMonad.Layout.Decoration as LD
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Roledex
import XMonad.Layout.Circle
import XMonad.Layout.OneBig
import XMonad.Layout.GridVariants
import XMonad.Layout.Roledex
import XMonad.Layout.Accordion
import XMonad.Layout.ToggleLayouts

import XMonad.Util.Run(spawnPipe, runProcessWithInput, runProcessWithInputAndWait, seconds)
import XMonad.Util.DunstSupport
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.MyNamedScratchpad
import XMonad.Util.NamedWindows
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.HandleEventHooks

import XMonad.Util.Performance
import XMonad.Layout.CachedLayout

black = "#4E4B42"
brightBlack = "#635F54"
gray = "#B4AF9A"
darkWhite = "#CDC8B0"
white = "#DAD4BB"
red = "#CC654C"
blue = "#3BA99F"

intellijCommand = "~/bin/idea"

applications = [
-- ("Vivaldi (Web browser)", "export GDK_DPI_SCALE=1.02; vivaldi"),
 ("Vivaldi (Web browser)", "vivaldi"),
 ("Nautilus (File browser)", "nautilus"),
 ("Emacs (Editor)", "emacs"),
 ("LINE", "wine '/home/jp21734/.wine/drive_c/users/jp21734/Local Settings/Application Data/LINE/bin/LineLauncher.exe'"),
 ("Configuration", "XDG_CURRENT_DESKTOP=GNOME gnome-control-center"),
 ("LibreOffice", "libreoffice"),
 ("JetBrains ToolBox", "~/bin/jetbrains-toolbox-1.14.5179/jetbrains-toolbox"),
 ("IntelliJ Idea", intellijCommand),
 ("PulseSecure", "/opt/pulsesecure/bin/pulseUI"),
 ("Slack", "slack"),
 ("Tweetdeck", webApplication "https://tweetdeck.twitter.com/"),
 ("YouTube", webApplication "https://youtube.com/"),
 ("DAZN", webApplication "https://dazn.com/")]

webApplication url = "vivaldi-stable --app=" ++ url

systemActions = [
 ("Reload", myrestart),
 ("Lock", spawn "gnome-screensaver-command --lock"),
 ("Suspend", spawn "systemctl suspend"),
 ("Logout", io (exitWith ExitSuccess)),
 ("Shutdown", spawn "systemctl poweroff"),
 ("Reboot", spawn "systemctl reboot")]

priorityDisplayEDIDs :: [EDID]
priorityDisplayEDIDs = [
 "00ffffffffffff00061044a000000000",
 "00ffffffffffff0010acb5414c323332",
 "00ffffffffffff0010acb7414c323332",
 "00ffffffffffff0010acb5414c333232"]

intelliJTerminalEnv =
  IntelliJTerminalEnvironment {
    homeDirectory = "/home/jp21734",
    XMonad.Actions.IntelliJTerminal.hook = onBottom
  }

myManageHookAll = manageHook gnomeConfig -- defaultConfig
                       <+> manageDocks
                       <+> myDocksManageHook
                       <+> myScratchpadsManageHook
                       <+> terminalManageHook myTerminal myTerminalActions
                       <+> ((fmap (L.isSuffixOf ".onBottom") appName) --> onBottom)
                       <+> (stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> onCenter' 0.1)
                       <+> ((isDialog <&&> (className =? "Gimp")) --> onCenter' 0.1)
                       <+> ((className =? "jetbrains-idea") <&&> (title =? "win0") --> doFloat)
                       <+> intelliJTerminalManageHook intelliJTerminalEnv
                       <+> ((appName =? "gnome-screenshot") --> doIgnore)
                       <+> ((className =? "Vivaldi-stable") <&&> (stringProperty "WM_WINDOW_ROLE" =? "pop-up") --> onCenter' 0.3)
--                       <+> (ask >>= \w -> liftX (debugWindow w))

--debugWindow w = do
--  appName <- runQuery appName w
--  className <- runQuery className w
--  spawn $ "echo '" ++ appName ++ ", " ++ className ++ "' >> /tmp/xmonad.managehook.debug"
--  spawn $ "xprop -id " ++ (show w) ++ "' >> /tmp/xmonad.managehook.debug"
--  return $ Endo $ \a -> a

myLayout = measureLayoutHook "myLayout" $ compositeTall (3/100) wide
  where wide = simpleWide (3/100)
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

tall = Tall 1 (3/100) (1/2)

myLogHook xmprocs = do
    xmobarLogHook xmprocs
    checkAndHandleDisplayChange moveMouseToLastPosition
    floatOnUp
    dunstLogHook
    terminalLogHook myTerminal myTerminalActions

xmobarLogHook xmprocs = withWindowSet (\s ->
    L.foldl (>>) def (map (\(i, xmproc) -> do
--      originalScreenIdToCurrentScreenIdMap <- originalScreenIdToCurrentScreenId priorityDisplayEDIDs
        j <- (\(OriginalDisplayIdToCurrentScreenId idToId) -> fromMaybe i $ M.lookup i idToId) <$> XS.get
        dynamicLogWithPP (multiScreenXMobarPP s j xmproc)) (L.zip [0..(L.length xmprocs)] xmprocs)))

--value_mask :: !CULong = (bit 2) (.|.) (bit 3)
myHandleEventHook =
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
    handleEventHook gnomeConfig <+>
    docksEventHook <+>
    (\e -> do
             logCurrentMouseLocation
             return (All True)) <+>
    myScratchpadsHandleEventHook <+>
    myTerminalActionHandleEventHook <+>
    (\e ->
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
             ifX (testBit ev_value_mask 6) $ windows (\s -> W.focusWindow ev_window s)
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
    (keepWindowSizeHandleEventHook $ stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog") <+>
    (keepWindowSizeHandleEventHook $ (isDialog <&&> (className =? "Gimp"))) <+>
    fullScreenEventHook <+>
    handleDoubleKeyPress [xK_Super_L, xK_Super_R] 300 (myNamedScratchpadAction "fzf_actions")

handleDoubleKeyPress metaKeys interval x e@(KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code, ev_time = time}) =
            withDisplay $ \dpy -> do
              s <- io $ keycodeToKeysym dpy code 0
              if L.elem s metaKeys  then do
                  RemapRequested r <- XS.get
                  if r then do
                      spawn $ "echo 'grabMetaKeys: " ++ (show e) ++ "' >> /tmp/xmonad.debug.event"
                      XS.put $ RemapRequested False
                      grabMetaKey metaKeys
                  else return ()
                  if t == keyPress then do
                      spawn $ "echo 'Press " ++ (show s) ++ "' >> /tmp/xmonad.debug.event"
                      LastMetaPress lastMetaPress <- XS.get
                      case lastMetaPress of
                        Just (lastKey, lastTime) -> do
                          let diffTime = time - lastTime
                          if lastKey == s && diffTime < interval then do
                            spawn $ "echo 'Press " ++ (show e) ++ " state = " ++ (show lastMetaPress) ++ "diff = " ++ (show $ time - lastTime) ++ "' >> /tmp/xmonad.debug.event"
                            x
                          else return ()
                        _ -> return ()
                      XS.put $ LastMetaPress $ Just (s, time)
                                 else return ()
                  return (All False)
              else do
                  spawn $ "echo 'Press " ++ (show s) ++ "' >> /tmp/xmonad.debug.event"
                  XS.put $ LastMetaPress $ Nothing
                  return (All True)
handleDoubleKeyPress _ _ _ e@(MappingNotifyEvent {}) = do
  spawn $ "echo 'Mapping " ++ (show e) ++ "' >> /tmp/xmonad.debug.event"
  XS.put $ RemapRequested True
  return (All True)
handleDoubleKeyPress metaKeys _ _ e = do
  RemapRequested r <- XS.get
  if r then do
      spawn $ "echo 'grabMetaKeys: " ++ (show e) ++ "' >> /tmp/xmonad.debug.event"
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
    -- ms <- io $ getModifierMapping dpy
    -- c <- io $ keycodeToKeysym dpy 207 0
    -- spawn $ "echo '" ++ (show mod4Mask) ++ "' >> /tmp/xmonad.debug.event"
    -- spawn $ "echo '" ++ (show c) ++ "' >> /tmp/xmonad.debug.event"
    -- spawn $ "echo '" ++ (show ms) ++ "' >> /tmp/xmonad.debug.event"
    return ()

myStartupHook =
    startupHook gnomeConfig <+>
    docksStartupHook <+>
    myDocksStartupHook <+>
    configureMouse <+>
    myrescreen priorityDisplayEDIDs <+>
    (grabMetaKey [xK_Super_L, xK_Super_R])

watch :: String -> String -> IO ()
watch cmd interval = spawn $ "while :; do " ++ cmd ++ "; sleep " ++ interval ++ "; done"

meta = mod4Mask
altMask = mod1Mask
alt = altMask
shft = shiftMask
ctrl = controlMask

bindKeys :: [(KeyMask, KeySym)] -> X () -> [((KeyMask, KeySym), X ())]
bindKeys keys x = L.map (\k -> (k, x)) keys

bindKey :: KeyMask -> KeySym -> X () -> [((KeyMask, KeySym), X ())]
bindKey mask key x = [((mask, key), x)]

systemKeys = [
  -- System actions
    ((mod4Mask, xK_q), myRunSelectedXTerminalAction systemActions)
  , ((mod1Mask .|. mod4Mask, xK_q), runActionSelected hidpiGSConfig systemActions)
  , ((mod4Mask, xK_r), withWindowSet $ \ws -> do
                         let sid = W.screen $ W.current ws
                         viewScreen 0
                         refresh
                         myrescreen priorityDisplayEDIDs
                         docksStartupHook
                         myDocksStartupHook
                         resetVirtualScreens
                         viewScreen sid) -- rescreen >>
  -- Screenshot
  , ((0, xK_Print), spawn "sh ~/.xmonad/screenshot.sh")
--         , ((controlMask, xK_Print), spawn "gnome-screenshot -c")
  , ((mod4Mask, xK_s), spawn "sh ~/.xmonad/screenshot.sh")
  , ((mod4Mask .|. shiftMask, xK_s), spawn "sh ~/.xmonad/screenshot.sh -a")
  ]

windowKeys = L.concat $ [
  -- Emacs binding
  -- Window $ Workspace
  -- Focus window
    bindKeys [(meta, xK_p)
             ,(meta, xK_Up)] $ windows floatAvoidFocusUp
  , bindKeys [(meta, xK_n)
             ,(meta, xK_Down)] $ windows floatAvoidFocusDown
  -- Focus workspace
  , bindKeys [(mod4Mask, xK_b)
             ,(mod4Mask, xK_Left)] $ do
      windowViewState <- XS.get
      case windowViewState of
        Normal -> prevWS'
        WindowView -> windows floatAvoidFocusUp
  , bindKeys [(mod4Mask, xK_f)
             ,(mod4Mask, xK_Right)] $ do
      windowViewState <- XS.get
      case windowViewState of
        Normal -> nextWS'
        WindowView -> windows floatAvoidFocusDown
  -- Shift window
  , bindKeys [(meta .|. shft, xK_p)
             ,(meta .|. shft, xK_Up)] $ windows floatAvoidSwapUp
  , bindKeys [(mod4Mask .|. shiftMask, xK_n)
             ,(mod4Mask .|. shiftMask, xK_Down)] $ windows W.swapDown
  , bindKeys [(meta .|. shft, xK_b)
             ,(meta .|. shft, xK_Left)] $ shiftToPrevWS' >> prevWS'
  , bindKeys [(meta .|. shft, xK_f)
             ,(meta .|. shft, xK_Right)] $ shiftToNextWS' >> nextWS'
  -- Master
  , bindKey meta xK_m $ windows W.focusMaster
  , bindKey (meta .|. shft) xK_m $ windows W.shiftMaster
  , bindKey (meta .|. shft .|. ctrl) xK_m $ windows W.swapMaster

  , [((mod4Mask .|. m, k), f i)
      | (i, k) <- zip originalWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
      , (f, m) <- [(greedyViewToWorkspace, 0), (shiftToWorkspace, shiftMask)]
    ]
  , [((mod4Mask .|. m, k), f i)
      | (i, k) <- zip workspaceFamilies $ [xK_1 .. xK_9] ++ [xK_0]
      , (f, m) <- [
        (\family -> submap . M.fromList $
                      [((0, subkey), greedyViewToFamilyWorkspace family workspace)
                        | (workspace, subkey)  <- zip originalWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
                      ], controlMask)
      , (\family -> submap . M.fromList $
                      [((0, subkey), shiftToFamilyWorkspace family workspace)
                        | (workspace, subkey) <- zip originalWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
                      ], (controlMask .|. shiftMask))
      ]
    ]
  ]

floatWindowKeys = [
    ((mod4Mask, xK_t), do
                         withWindowSet $ \s -> spawn $ "echo '" ++ (show $ sortedFloats' s) ++ "' >> /tmp/xmonad.debug.floats"
                         windows floatFocusDown
                         withWindowSet $ \s -> spawn $ "echo '" ++ (show $ W.integrate' $ W.stack $ W.workspace $ W.current s) ++ "' >> /tmp/xmonad.debug.floats")
  , ((mod4Mask .|. shiftMask, xK_t), windows floatFocusUp)
  , ((mod4Mask .|. controlMask, xK_t), withFocused $ windows . W.sink) -- %! Push window back into tiling
  ]

screenKeys = L.concat $ [
    bindKeys [(meta .|. alt, xK_p)
             ,(meta .|. shft, xK_space)] $ prevVirtualScreen >> moveMouseToLastPosition
  , bindKeys [(meta .|. alt, xK_n)
             ,(meta, xK_space)] $ nextVirtualScreen >> moveMouseToLastPosition
  , bindKey (meta .|. shft .|. alt) xK_p $ shiftPrevRootScreen >> prevVirtualScreen >> moveMouseToLastPosition
  , bindKey (meta .|. shft .|. alt) xK_n $ shiftNextRootScreen >> nextVirtualScreen >> moveMouseToLastPosition
  -- M+Alt 1~0: View screen
  -- M+Alt+Ctrl 1~0: Greedy view to screen
  -- M+Alt+Shift 1~0: Shift to screen
  , [
          ((mod4Mask .|. m, k), f i)
            | (i, k) <- zip [1..9] [xK_1 .. xK_9]
            , (f, m) <-[(viewToScreen, mod1Mask), (greedyViewToScreen, mod1Mask .|. controlMask), (shiftToScreen, mod1Mask .|. shiftMask)]
    ]
  ]

virtualScreenKeys = [
    ((mod4Mask, xK_x), createVirtualScreen $ ((Mirror $ simpleWide (3/100)) ||| (simpleWide (3/100))))
  , ((mod4Mask .|. shiftMask, xK_x), resetVirtualScreen)
  , ((mod4Mask .|. mod1Mask, xK_d), sendScreenMessage NextLayout)
  , ((mod4Mask .|. mod1Mask, xK_j), sendScreenMessage Shrink)
  , ((mod4Mask .|. mod1Mask, xK_l), sendScreenMessage Expand)
  , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_j), sendScreenMessage $ ResizeAnotherSide Expand)
  , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_l), sendScreenMessage $ ResizeAnotherSide Shrink)
  , ((mod4Mask .|. mod1Mask, xK_b), prevChildScreen)
  , ((mod4Mask .|. mod1Mask, xK_f), nextChildScreen)
  ]

layoutKeys = [
  -- Layout
    ((mod4Mask, xK_d), sendMessage NextLayout)
  , ((mod4Mask .|. shiftMask, xK_comma ), sendMessage NewCellAtLeft)
  , ((mod4Mask .|. shiftMask, xK_period), sendMessage NewCellAtRight)
  -- Struts
  , ((mod4Mask, xK_h), docksOnBottom >> (sendMessage ToggleStruts))
  , ((mod4Mask .|. shiftMask, xK_h), sendMessage $ XMonad.Layout.MyMultiToggle.Toggle TitleTransformer)

  -- Window view
  , ((mod4Mask, xK_v), do
      windowViewState <- XS.get
      XS.put Normal
      case windowViewState of
        WindowView -> broadcastMessage Focus >> refresh
        -- Full screen
        Normal -> sendMessage ToggleLayout)
  , ((mod4Mask .|. shiftMask, xK_v), do
        broadcastMessage View
        XS.put WindowView
        refresh)
  ]

sizingKeys = [
    ((meta,          xK_j), whenX (xnot $ locateFloat $ avoidStrutsFloat <+> onLeftTest)
                              $ sendMessage Shrink)
  , ((meta,          xK_l), whenX (xnot $ locateFloat $ avoidStrutsFloat <+> onRightTest)
                              $ sendMessage Expand)
  , ((meta,          xK_i), whenX (xnot $ locateFloat $ avoidStrutsFloat <+> onTopTest)
                              $ sendMessage $ DelegateMessage $ SomeMessage Shrink)
  , ((meta,          xK_k), whenX (xnot $ locateFloat $ onBottom)
                              $ sendMessage $ DelegateMessage $ SomeMessage Expand)

  , ((meta .|. shft, xK_i), sendMessage $ DelegateMessage $ SomeMessage $ ResizeAnotherSide Expand)
  , ((meta .|. shft, xK_k), sendMessage $ DelegateMessage $ SomeMessage $ ResizeAnotherSide Shrink)
  , ((meta .|. shft, xK_j), sendMessage $ ResizeAnotherSide Expand)
  , ((meta .|. shft, xK_l), sendMessage $ ResizeAnotherSide Shrink)

  , ((meta .|. shft, xK_r), sendMessage ResetSize)
  ]

scratchpadKeys = [
  -- Scratchpad
    ((mod4Mask, xK_F4), myNamedScratchpadAction "rhythmbox")
  , ((mod4Mask, xK_Return), myNamedScratchpadAction "mainterm")
  , ((mod4Mask, xK_F9), myNamedScratchpadAction "艦これ")
  , ((mod4Mask, xK_F10), myNamedScratchpadAction "bunnaru")
  , ((mod4Mask, xK_bracketleft), myNamedScratchpadAction "term1")
  , ((mod4Mask, xK_bracketright), myNamedScratchpadAction "term2")
  , ((mod4Mask .|. shft, xK_bracketleft), myNamedScratchpadAction "termL")
  , ((mod4Mask .|. shft, xK_bracketright), myNamedScratchpadAction "termR")
  , ((mod4Mask .|. controlMask, xK_bracketleft), myNamedScratchpadAction "jshell1")
  , ((mod4Mask .|. controlMask, xK_bracketright), myNamedScratchpadAction "jshell2")

  , ((mod4Mask .|. controlMask, xK_F7), toggleScrachpadAction $ L.reverse myScratchpads)
  , ((mod4Mask .|. controlMask, xK_F8), showOrHideScratchpads myScratchpads True)
  , ((mod4Mask .|. controlMask .|. shiftMask, xK_F8), showOrHideScratchpads myScratchpads False)
  , ((mod4Mask .|. controlMask, xK_F9), toggleScrachpadAction myScratchpads)
  ]

terminalActionKeys = [
  -- TerminalAction
    ((meta, xK_w),           smartGreedyViewSelectedWindowTerminalAction windowPredicates)
  , ((meta .|. ctrl, xK_w),  greedyViewSelectedWindowTerminalAction      windowPredicates)
  , ((meta .|. shft, xK_w),  shiftSelectedWindowTerminalAction           windowPredicates)
  , ((meta, xK_e),           spawnAppSelectedTerminalAction' applications)
  , ((meta, xK_at),          runOpenDashboardTerminalAction)
  , ((meta .|. shft, xK_at), runOpenITerminalAction)
  , ((meta, xK_colon),       openIntelliJTerminalAction)
  , ((meta, xK_semicolon),   runOpenBrowserHistoryTerminalAction)
  , ((meta, xK_c),           runCopyFromClipboardHistoryTerminalAction)
  , ((meta .|. ctrl, xK_c),  runOnePasswordTerminalAction)
  ]
  where windowPredicates = [
          ("All workspaces", anyWorkspacePredicate),
          ("Visible workspaces", visibleWorkspacesPredicate),
          ("Workspace for current family", anyWorkspaceInCurrentWorkspaceFamilyPredicate)]

functionKeys = [
  -- Functions
    ((mod4Mask              , xK_F1), spawn "sh ~/.xmonad/audio_mute.sh")
  , ((mod4Mask              , xK_F2), spawn "sh ~/.xmonad/audio_down.sh")
  , ((mod4Mask              , xK_F3), spawn "sh ~/.xmonad/audio_up.sh")
  , ((mod4Mask .|. shiftMask, xK_F2), spawn "sh ~/.xmonad/audio_prev.sh")
  , ((mod4Mask .|. shiftMask, xK_F3), spawn "sh ~/.xmonad/audio_next.sh")
  -- F4: rhythmbox
  , ((mod4Mask              , xK_F5), spawn "sh ~/.xmonad/system_scripts/bright/down.sh")
  , ((mod4Mask              , xK_F6), spawn "sh ~/.xmonad/system_scripts/bright/up.sh")
  , ((mod4Mask              , xK_F7), spawn "intel-pstate-utils-update-freq min up")
  , ((mod4Mask .|. shiftMask, xK_F7), spawn "intel-pstate-utils-update-freq min down")
  , ((mod4Mask              , xK_F8), spawn "intel-pstate-utils-update-freq max up")
  , ((mod4Mask .|. shiftMask, xK_F8), spawn "intel-pstate-utils-update-freq max down")
  -- F9: 艦これ
  -- F10: 文ある
  ]

utilKeys = [
  -- GridSelected
    ((mod1Mask .|. mod4Mask, xK_w),                               goToSelected'  anyWorkspaceInCurrentWorkspaceFamilyPredicate hidpiGSConfig)
  , ((mod1Mask .|. mod4Mask .|. controlMask, xK_w),               goToSelected'  anyWorkspacePredicate                         hidpiGSConfig)
  , ((mod1Mask .|. mod4Mask .|. shiftMask, xK_w),                 shiftSelected' anyWorkspaceInCurrentWorkspaceFamilyPredicate hidpiGSConfig)
  , ((mod1Mask .|. mod4Mask .|. controlMask .|. shiftMask, xK_w), shiftSelected' anyWorkspacePredicate                         hidpiGSConfig)
  , ((mod1Mask .|. mod4Mask, xK_e),                               spawnAppSelected hidpiGSConfig applications)
  ------------------------------------------------------------------------------------------------------------------------------------

  , ((mod4Mask .|. shiftMask, xK_e), spawn "gmrun")
  , ((mod4Mask, xK_g), selectSearchBrowser "/usr/bin/vivaldi" google)
  , ((mod4Mask, xK_backslash), launchIntelliJTerminal intelliJTerminalEnv)
  ]

main = do
    -- Display
    runProcessWithInputAndWait "sh" ["-c", "sh '/home/jp21734/.xmonad/auto_detect_display.sh' >> /tmp/debug"] "" (seconds 1)

    -- Keyboard and Mouse
    spawn "xhost +SI:localuser:root; sleep 1; sudo xkeysnail --watch -q ~/config.py & sleep 3; xset r rate 250 50; xset q >> /tmp/xset.debug"
    spawn "sudo libinput-gestures"

    -- Desktop
    spawn "sleep 3; feh --bg-fill ~/Pictures/Lza1qCX.png"

    -- Applets
    spawn "nm-applet" -- ネット接続のアプレットを起動
    spawn "fcitx"
    -- gnome-sound-appletのアイコンが黒一色でない場合は--transparent trueにすると統一感があっていいです。 -- GNOMEのトレイを起動 -- XXX(sleep 2): #6: Trayer broken with nautilus
    spawn "sleep 5; killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 5 --widthtype percent --transparent true --tint 0x4E4B42 --height 30 --alpha 0 --monitor 0"

    -- Workaround for Java apps
    spawn "wmname LG3D"

--    spawn "compton -b --config ~/.comptonrc"

    io (threadDelay (2 * 1000 * 1000))
    numDisplayStr <- runProcessWithInput "sh" ["-c", "xrandr --query | grep -c '\\bconnected\\b'"] ""
    let numDisplay = read numDisplayStr :: Int
    spawn $ "echo '" ++ (show numDisplay) ++ "' > /tmp/test"
    spawn $ "xrandr --query | grep -c '\\bconnected\\b' >> /tmp/test"
    xmprocs <- mapM (\displayId -> spawnPipe $ "/usr/bin/xmobar " ++ (if displayId == 0 then "" else "-p Top -x " ++ (show displayId)) ++ " ~/.xmobarrc") [0..numDisplay-1]

    spawn "gnome-screensaver"
    spawn "pulseeffects --gapplication-service"
    spawn "killall dunst"

    spawn "xrandr --output eDP-1 --brightness 1 --gamma 1.05:1.05:1.095"

    spawn "CM_MAX_CLIPS=10000 CM_DIR=$HOME CM_SELECTIONS=clipboard CM_IGNORE_WINDOW=xmonad.terminal.action.one.password clipmenud"

    spawn "~/.xmonad/system_scripts/bright/sync.sh"
    
--    spawn $ "echo '" ++ (show $ mkToggleInitial (single TitleTransformer) TitleTransformer $ myLayout) ++ "' >> /tmp/xmonad.debug.layout"
    xmonad $ gnomeConfig
        { manageHook = myManageHookAll
--        , layoutHook =  myLayoutHookAll
        , layoutHook =  measureLayoutHook "layoutHook" $ myLayoutHookAll
        , logHook = measure "logHook" $ myLogHook xmprocs
        , handleEventHook = \e -> measure "handleEventHook" $ myHandleEventHook e
        , startupHook = myStartupHook
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = 4
        , normalBorderColor  = blue
        , focusedBorderColor = red
        , focusFollowsMouse = False -- マウスの移動でフォーカスが映らないように
        , clickJustFocuses = False
        , XMonad.Core.workspaces = myWorkspaces
        } `additionalKeys` (L.concat $ [
          systemKeys
        , windowKeys
        , floatWindowKeys
        , screenKeys
        , virtualScreenKeys
        , layoutKeys
        , sizingKeys
        , functionKeys
        , terminalActionKeys
        , scratchpadKeys
        , utilKeys
        ]) `additionalKeys`
        [

        -- Arrow key
        -- フォーカスの移動
        -- スワップ
        -- ワーススペースの移動
--        , ((mod4Mask .|. controlMask, xK_Up), prevWS')
--        , ((mod4Mask .|. controlMask, xK_Left), prevWS')
--        , ((mod4Mask .|. controlMask, xK_Down), nextWS')
--        , ((mod4Mask .|. controlMask, xK_Right), nextWS')
        -- ワーススペース間のスワップ
--        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up), shiftToPrevWS' >> prevWS')
--        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Left), shiftToPrevWS' >> prevWS')
--        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down), shiftToNextWS' >> nextWS')
--        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Right), shiftToNextWS' >> nextWS')






--        , ((mod4Mask, xK_s), scratchpadSelected hidpiGSConfig myScratchpads)
        ------------------------------------------------------------------------------------------------------------------------------------


        -- CopyWindow WIP
--        , ((mod4Mask, xK_a), windows copyToAll)
--        , ((mod4Mask .|. shiftMask, xK_a), killAllOtherCopies)
--        , ((mod4Mask, xK_z), showAllWindow)



--        , ((mod4Mask, xK_c), getDurations >>= \d -> spawn $ "echo '" ++ (show d) ++ "' >> /tmp/xmonad.debug.perf")
--        , ((mod4Mask .|. controlMask, xK_c), resetDurations)
        ] `additionalKeysP`
        [
        -- 輝度・ボリューム周り
          ("<XF86MonBrightnessDown>", spawn "sh ~/.xmonad/system_scripts/bright/down.sh")
        , ("<XF86MonBrightnessUp>", spawn "sh ~/.xmonad/system_scripts/bright/up.sh")
        , ("<XF86KbdBrightnessDown>", spawn "sh ~/.xmonad/kbd_bright_down.sh")
        , ("<XF86KbdBrightnessUp>", spawn "sh ~/.xmonad/kbd_bright_up.sh")
        , ("<XF86AudioLowerVolume>", spawn "sh ~/.xmonad/audio_down.sh")
        , ("<XF86AudioRaiseVolume>", spawn "sh ~/.xmonad/audio_up.sh")
        , ("M4-<XF86AudioLowerVolume>", spawn "sh ~/.xmonad/audio_prev.sh")
        , ("M4-<XF86AudioRaiseVolume>", spawn "sh ~/.xmonad/audio_next.sh")
        , ("<XF86AudioMute>",        spawn "sh ~/.xmonad/audio_mute.sh")
--        , ("M4-<XF86AudioPlay>", myNamedScratchpadAction "rhythmbox")
--        , ("<XF86AudioLowerVolume>", setMute(False) >> lowerVolume 3 >> return ())
--        , ("<XF86AudioRaiseVolume>", setMute(False) >> raiseVolume 3 >> return ())
--        , ("<XF86AudioMute>",        setMute(False) >> setVolume 50   >> return ()) -- toggleMuteで問題がなければそうすると良いです。
        , ("<XF86LaunchA>", showOrHideScratchpads myScratchpads False)
        , ("<XF86LaunchB>", showOrHideScratchpads myScratchpads True)
        ] `removeKeys`
        [
          (mod4Mask .|. shiftMask, xK_q)
        , (mod4Mask .|. shiftMask, xK_slash)
--        , (mod4Mask, xK_q)
        ] `additionalMouseBindings` [
          ((mod4Mask, button1), \w -> focus w >> mouseMoveWindow w)
        , ((mod4Mask .|. controlMask, button1), \w -> focus w >> MR.mouseResizeWindow w
                                                              >> windows W.shiftMaster)
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
        ]

-- Libraries

------------------------------------------------------------------------------------------
-- XMonad utils
------------------------------------------------------------------------------------------

-- | Modify the @WindowSet@ in state with no special handling.
--modifyWindowSet :: (WindowSet -> WindowSet) -> X ()
--modifyWindowSet f = modify $ \xst -> xst { windowset = f (windowset xst) }

ifX :: Bool -> X() -> X()
ifX cond whenTrue = if cond then whenTrue else return ()

xnot :: X Bool -> X Bool
xnot x = not <$> x

caseMaybeJust :: Maybe a -> (a -> X ()) -> X ()
caseMaybeJust m f =
  case m of
    Just a -> f a
    Nothing -> return ()

doForJust f m = caseMaybeJust m f

isDialog = ask >>= \w -> liftX $ do
  desk <- getAtom "_NET_WM_WINDOW_TYPE_DIALOG"
  mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
  case mbr of
    Just rs -> return $ any (== desk) (map fromIntegral rs)
    _       -> return False
------------------------------------------------------------------------------------------
-- XMonad utils
------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------
-- ManageHook
------------------------------------------------------------------------------------------
strutsOffsetRatioTop = 0.02

onTop = onTop' 0
onTop' spaceRatio = onTop'' spaceRatio spaceRatio
onTop'' spaceRatioV spaceRatioH = (customFloating $ W.RationalRect spaceRatioV (spaceRatioH + strutsOffsetRatioTop) (1-spaceRatioV*2) (0.5-spaceRatioH*2-strutsOffsetRatioTop))
onBottom = onBottom' 0
onBottom' spaceRatio = onBottom'' spaceRatio spaceRatio
onBottom'' spaceRatioV spaceRatioH = (customFloating $ W.RationalRect spaceRatioV (0.5+spaceRatioH) (1-spaceRatioV*2) (0.5-spaceRatioH*2))
onCenter = onCenter' 0
onCenter' spaceRatio = onCenter'' spaceRatio spaceRatio
onCenter'' spaceRatioV spaceRatioH = (customFloating $ W.RationalRect spaceRatioV (spaceRatioH + strutsOffsetRatioTop) (1-spaceRatioV*2) (1-spaceRatioH*2-strutsOffsetRatioTop))

onTopTest = customFloating $ W.RationalRect 0 0 1 0.5

onLeft = onLeft' 0
onLeft' spaceRatio = onLeft'' spaceRatio spaceRatio
onLeft'' spaceRatioV spaceRatioH = (customFloating $ W.RationalRect spaceRatioV (spaceRatioH + strutsOffsetRatioTop) (0.5-spaceRatioV*2) (1-spaceRatioH*2-strutsOffsetRatioTop))
onLeftTest = customFloating $ W.RationalRect 0 0 0.5 1
onRightTest = customFloating $ W.RationalRect 0.5 0 0.5 1

avoidStrutsFloat = do
  win <- ask
  (sid, r) <- liftX $ floatLocation win
  doF $ \ws ->
      case L.find ((sid ==) . W.screen) $ W.screens ws of
        Just sc ->
          let Rectangle sx sy sw sh = screenRect $ W.screenDetail sc in
          case M.lookup win $ W.floating ws of
            Just (W.RationalRect x y w h) ->
              W.float win (W.RationalRect x (y + (30/(fromIntegral sh))) w (h - (30/fromIntegral sh))) ws
--              W.float win (W.RationalRect x (y + (strutsOffsetRatioTop)) w (h - strutsOffsetRatioTop)) ws
--              ws
            _ -> ws
        _ -> ws
------------------------------------------------------------------------------------------
-- ManageHook
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- Support for docks
-- Desktop lowest, Docks higher than desktop but lower than other windows
------------------------------------------------------------------------------------------
myDocksStartupHook = docksOnBottom

myDocksManageHook = checkDock --> (liftX docksOnBottom >> mempty)

docksOnBottom = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    (_,_,wins) <- io $ queryTree dpy rootw
    docks <- filterM (runQuery checkDockOnly) wins
    desks <- filterM (runQuery $ checkDesktopOnly <&&> checkBackgroundDesktop) wins
--    forM (docks ++ desks) $ \win -> do
--      name <- runQuery className win
--      withWindowAttributes dpy win $ \(WindowAttributes {wa_x = x, wa_y = y, wa_width = w, wa_height = h}) -> do
--        (_, p, cs) <- io $ queryTree dpy win
--        let s = (show x) ++ "," ++ (show y) ++ "," ++ (show w) ++ "," ++ (show h)
--        pn <- runQuery className p
--        spawn $ "echo '" ++ ((show p) ++ "," ++ (show pn)) ++ " -> " ++ (show win) ++ ":" ++ (show name) ++ "," ++ s ++ (" -> " ++ (show cs)) ++ "' >> /tmp/xmonad.debug.docks"
    io $ L.foldr (>>) (return ()) $ L.map (lowerWindow dpy) $ L.reverse docks
    io $ L.foldr (>>) (return ()) $ L.map (lowerWindow dpy) $ L.reverse desks

checkDockOnly = ask >>= \w -> liftX $ do
  dock <- getAtom "_NET_WM_WINDOW_TYPE_DOCK"
  mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
  case mbr of
    Just rs -> return $ any (== dock) (map fromIntegral rs)
    _       -> return False
checkDesktopOnly = ask >>= \w -> liftX $ do
  desk <- getAtom "_NET_WM_WINDOW_TYPE_DESKTOP"
  mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
  case mbr of
    Just rs -> return $ any (== desk) (map fromIntegral rs)
    _       -> return False
checkBackgroundDesktop = className =? "Nautilus"
------------------------------------------------------------------------------------------
-- Support for docks
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------------------------
javaHome = "~/.sdkman/candidates/java/current"
jshellPath = javaHome ++ "/bin/jshell"

myScratchpads :: [NamedScratchpad]
myScratchpads = [
    terminalScratchpad "mainterm" Nothing $ onCenter' 0.01
  , terminalScratchpad "term1" Nothing (avoidStrutsFloat <+> onTopTest)
  , terminalScratchpad "term2" Nothing onBottom
  , terminalScratchpad "termL" Nothing (avoidStrutsFloat <+> onLeftTest)
  , terminalScratchpad "termR" Nothing (avoidStrutsFloat <+> onRightTest)
--  , terminalScratchpad "term2" Nothing onBottom
  , terminalScratchpad "jshell1" (Just jshellPath) (avoidStrutsFloat <+> onTopTest)
  , terminalScratchpad "jshell2" (Just jshellPath) onBottom
  , NS "bunnaru"
           "google-chrome --renderer-process-limit=1 --new-window --app=http://www.dmm.com/netgame/social/-/gadgets/=/app_id=798209/"
           (appName =? "www.dmm.com__netgame_social_-_gadgets_=_app_id=798209")
           (customFloating $ W.RationalRect 0 0.4 0.55 0.6)
  , NS "艦これ"
           "google-chrome --renderer-process-limit=1 --new-window --app=http://www.dmm.com/netgame/social/-/gadgets/=/app_id=854854/"
           (appName =? "www.dmm.com__netgame_social_-_gadgets_=_app_id=854854")
           (customFloating $ W.RationalRect 0.55 0.4 0.45 0.6)
  , NS "rhythmbox"
           "rhythmbox"
           (className =? "Rhythmbox")
           onCenter
  , terminalScratchpad "fzf_actions" (Just "\"zsh -c '. $HOME/.zshrc; _fzf_actions_then_echo | xsel -b -i'\"") $ onCenter'' 0.1 0.2
 ]

myScratchpadsManageHook = namedScratchpadManageHook myScratchpads
myScratchpadsHandleEventHook =
    namedScratchpadHandleEventHook myScratchpads <+>
    (keepWindowSizeHandleEventHook $ intelliJTerminalQuery)

myNamedScratchpadAction = myNamedScratchpadActionInternal myScratchpads

myNamedScratchpadActionMaybe mns =
  whenJust mns $ \ns -> myNamedScratchpadAction $ name ns

------------------------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- Float aware focus
------------------------------------------------------------------------------------------

floatAvoidFocusUp, floatAvoidFocusDown :: Ord a => W.StackSet i l a s sd -> W.StackSet i l a s sd
floatAvoidFocusUp stackSet = W.modify' (floatAvoidFocusUp' stackSet) stackSet
floatAvoidFocusDown stackSet = W.modify' (floatAvoidFocusDown' stackSet) stackSet

floatAvoidFocusUp', floatAvoidFocusDown' :: Ord a => W.StackSet i l a s sd -> W.Stack a -> W.Stack a
floatAvoidFocusUp' stackSet stack@(W.Stack t (l:ls) rs) =
    if M.member l $ W.floating stackSet then
        let W.Stack t' ls' rs' = floatAvoidFocusUp' stackSet (W.Stack t ls rs) in
          W.Stack t' (l:ls') rs'
    else W.Stack l ls (t:rs)
floatAvoidFocusUp' stackSet stack@(W.Stack t [] rs) = do
    let (x:xs) = reverse rs
    if M.member x $ W.floating stackSet then do
      let W.Stack t' ls' rs' = floatAvoidFocusUp' stackSet (W.Stack t [] $ reverse xs)
      W.Stack t' (x:ls') rs'
    else
      W.Stack x (xs ++ [t]) []

--floatAvoidFocusUp' stackSet (W.Stack t ls rs) = do
--  let (lf, ls') = L.partition (isFloat stackSet) ls
--  let (rf, rs') = L.partition (isFloat stackSet) rs
--  let W.Stack t' nls nrs = W.focusUp' $ W.Stack t ls' rs'
--  W.Stack t' (lf ++ nls) (rf ++ nrs)
floatAvoidFocusUp' stackSet stack@(W.Stack t [] []) = stack

floatAvoidFocusDown' stackSet = reverseStack . (floatAvoidFocusUp' stackSet) . reverseStack

floatAvoidSwapUp stackSet = W.modify' (floatAvoidSwapUp' stackSet) stackSet
floatAvoidSwapUp' stackSet stack@(W.Stack t ls rs) =
  if isFloat stackSet t then stack
  else do
    let (lf, ls') = span (isFloat stackSet) ls
    let W.Stack t' nls nrs = swapUp' $ W.Stack t ls' rs
    W.Stack t' nls ((reverse lf) ++ nrs)

swapUp' :: W.Stack a -> W.Stack a
swapUp'  (W.Stack t (l:ls) rs) = W.Stack t ls (l:rs)
swapUp'  (W.Stack t []     rs) = W.Stack t (reverse rs) []

floatFocusUp, floatFocusDown :: Ord a => W.StackSet i l a s sd -> W.StackSet i l a s sd
floatFocusUp stackSet = W.modify' (floatFocusUp' stackSet) stackSet
floatFocusDown stackSet = W.modify' (floatFocusDown' stackSet) stackSet

floatFocusUp', floatFocusDown' :: Ord a => W.StackSet i l a s sd -> W.Stack a -> W.Stack a
floatFocusUp' = floatFocusNext . reverse . sortedFloats

floatFocusDown' = floatFocusNext . sortedFloats

sortedFloats stackSet = L.map fst $ sortedFloats' stackSet
sortedFloats' stackSet =
    L.sortBy comparator floats
        where floats = M.assocs $ W.floating stackSet
              comparingX = comparing $ \(wid, W.RationalRect x y w h) -> x
              comparingY = comparing $ \(wid, W.RationalRect x y w h) -> y
              comparingW = comparing $ \(wid, W.RationalRect x y w h) -> w
              comparingH = comparing $ \(wid, W.RationalRect x y w h) -> h
              comparingWid = comparing $ \(wid, W.RationalRect x y w h) -> wid
              comparator = comparingY `andThen` comparingX `andThen` comparingH `andThen` comparingW `andThen` comparingWid

floatFocusNext floats stack@(W.Stack t ls rs) = do
    let ws = W.integrate stack
    let fs = L.reverse $ L.filter (\w -> L.elem w ws) $ floats
    if fs == [] || fs == [t] then stack
    else do
      let fs' = takeWhile (/= t) fs
      let w = if fs' == [] then last fs else last fs'
      W.Stack w (L.delete w ls) $ t:(L.delete w rs)

andThen cmp1 cmp2 a b = do
  let c = cmp1 a b
  if c == EQ then cmp2 a b
  else c

floatOnUp = withWindowSet(\s -> do
--  before <- gets windowset
--  caseMaybeJust (W.stack $ W.workspace $ W.current before) $
--    \(W.Stack t ls rs) -> spawn $ "echo 'Current: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
  caseMaybeJust (W.stack $ W.workspace $ W.current s) $
    \(W.Stack t ls rs) -> do
      if isFloat s t then
          if (L.filter (isFloat s) ls) == [] then return ()
          else focusedFloatOnUp
      else do
        let (rf, rs') = L.partition (isFloat s) rs
        let (lf, ls') = L.partition (isFloat s) $ L.dropWhile (isFloat s) ls
        if (rf ++ lf) == [] then return ()
        else floatOnUp')
--  before <- gets windowset
--  case W.stack $ W.workspace $ W.current before of
--    Just (W.Stack t ls rs) -> spawn $ "echo 'Current: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
--    Nothing -> return ())

floatOnUp' = do
--  before <- gets windowset
--  caseMaybeJust (W.stack $ W.workspace $ W.current before) $
--    \(W.Stack t ls rs) -> spawn $ "echo 'Before: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
  modifyWindowSet (\s -> W.modify' (\stack@(W.Stack t ls rs) -> do
    let (rf, rs') = L.partition (isFloat s) $ L.reverse rs
    let lf' = L.takeWhile (isFloat s) ls
    let (lf, ls') = L.partition (isFloat s) $ L.dropWhile (isFloat s) ls
    if (rf ++ lf) == [] then stack
    else W.Stack t (rf ++ lf' ++ lf ++ ls') $ reverse rs') s)
--  before <- gets windowset
--  case W.stack $ W.workspace $ W.current before of
--    Just (W.Stack t ls rs) -> spawn $ "echo 'After: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
--    Nothing -> return ()

focusedFloatOnUp = do
--  before <- gets windowset
--  case W.stack $ W.workspace $ W.current before of
--    Just (W.Stack t ls rs) -> spawn $ "echo 'Before: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
--    Nothing -> return ()
  windows (\s -> W.modify' (\stack@(W.Stack t ls rs) -> do
    let (lf, ls') = L.partition (isFloat s) ls
    if lf == [] then stack
--    else W.Stack t ls' $ rs ++ (reverse lf)) s)
    else W.Stack t ls' $ (reverse lf) ++ rs) s)
--  before <- gets windowset
--  case W.stack $ W.workspace $ W.current before of
--    Just (W.Stack t ls rs) -> spawn $ "echo 'After: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
--    Nothing -> return ()

isFloat stackSet window = M.member window $ W.floating stackSet

-- | reverse a stack: up becomes down and down becomes up.
reverseStack :: W.Stack a -> W.Stack a
reverseStack (W.Stack t ls rs) = W.Stack t rs ls

locateFloat managehook = withWindowSet (\s -> do
  case W.peek s of
    Just t ->
      if isFloat s t then do
        endo <- runQuery managehook t
        windows $ appEndo endo
        return True
      else
        return False
    Nothing -> return False
  )


------------------------------------------------------------------------------------------
-- WorkspaceFamily
------------------------------------------------------------------------------------------
notSP :: X (WindowSpace -> Bool)
notSP = return $ pureNotSP
pureNotSP :: WindowSpace -> Bool
pureNotSP = ("NSP" /=) . W.tag
pureNotSP' = ("NSP" /=)
currentWorkspaceFamily currentFamily = return (\ws -> L.isSuffixOf ("_" ++ currentFamily) $ W.tag ws)

currentFamilyId ws = toFamilyId $ W.tag $ W.workspace $ W.current ws
toFamilyId = drop 2
currentWorkspaceId ws = toWorkspaceId $ W.tag $ W.workspace $ W.current ws
toWorkspaceId id = [ head id ]
nextWS' :: X ()
nextWS' = doOnCurrentWorkspaceFamily $ moveTo Next
prevWS' :: X ()
prevWS' = doOnCurrentWorkspaceFamily $ moveTo Prev

shiftToNextWS' :: X()
shiftToNextWS' = doOnCurrentWorkspaceFamily $ shiftTo Next
shiftToPrevWS' :: X()
shiftToPrevWS' = doOnCurrentWorkspaceFamily $ shiftTo Prev

doOnCurrentWorkspaceFamily f = withWindowSet $ \s -> do
  let wsid = W.tag $ W.workspace $ W.current s
  ifX (pureNotSP' wsid) $ f $ WSIs $ currentWorkspaceFamily $ toFamilyId wsid

originalWorkspaces = map show ([1 .. 9 :: Int] ++ [0])
workspaceFamilies = map show ([1 .. 9 :: Int] ++ [0])
myWorkspaces = expandWorkspacesToFamily workspaceFamilies originalWorkspaces
expandWorkspacesToFamily families ws = concat $ map toFamilies ws
  where toFamilies wsid = map (\fid -> wsid ++ "_" ++ fid) families

data FamilyWorkspaceMap = FamilyWorkspaceMap (M.Map (ScreenId, String) String) deriving Typeable
instance ExtensionClass FamilyWorkspaceMap where
  initialValue = FamilyWorkspaceMap M.empty

greedyViewToWorkspace workspaceId =
    withWindowSet(\s -> do
      let familyId = currentFamilyId s
--      io $ appendFile "/tmp/xmonad.debug" $ workspaceId ++ "_" ++ (show sid)
      windows $ W.greedyView $ workspaceId ++ "_" ++ familyId
    )
greedyViewToFamily familyId =
    withWindowSet(\s -> do
      FamilyWorkspaceMap familyToWorkspace <- XS.get
      XS.put $ FamilyWorkspaceMap $ M.insert (W.screen $ W.current s, currentFamilyId s) (currentWorkspaceId s) familyToWorkspace
      case M.lookup (W.screen $ W.current s, familyId) familyToWorkspace of
        Just workspaceId -> greedyViewToFamilyWorkspace familyId workspaceId
        Nothing -> greedyViewToFamilyWorkspace familyId $ currentWorkspaceId s
    )
greedyViewToFamilyWorkspace familyId workspaceId = do
--    io $ appendFile "/tmp/xmonad.debug" $ workspaceId ++ "_" ++ familyId
    windows $ W.greedyView $ workspaceId ++ "_" ++ familyId
shiftToWorkspace workspaceId =
    withWindowSet(\s -> do
      let familyId = currentFamilyId s
--      io $ appendFile "/tmp/xmonad.debug" $ workspaceId ++ "_" ++ (show sid)
      windows (W.shift (workspaceId ++ "_" ++ familyId))
    )
shiftToFamily familyId =
    withWindowSet(\s -> shiftToFamilyWorkspace familyId $ currentWorkspaceId s)
shiftToFamilyWorkspace familyId workspaceId = do
--    io $ appendFile "/tmp/xmonad.debug2" $ workspaceId ++ "_" ++ familyId
    windows (W.shift (workspaceId ++ "_" ++ familyId))

multiScreenXMobarPP windowSet screenId xmproc = xmobarPP
                        { ppOutput = \t -> hPutStrLn xmproc $ (screenIds windowSet screenId) ++ " | " ++ (fallbackIfNoScreen (\ws -> \sid -> \fid -> fid) windowSet screenId) ++ " | " ++ t
                        , ppTitle = \t -> ""
                        , ppSep             = " | "
                        , ppExtras = [ titleOfScreenId windowSet screenId ]
                        , ppCurrent = fallbackIfNoScreen (showOnlyWorkspaceFor $ currentOfScreenId False) windowSet screenId
                        , ppVisible = fallbackIfNoScreen visibleOfScreenId windowSet screenId
                        , ppHidden = fallbackIfNoScreen (showOnlyWorkspaceFor $ \ws -> \sid -> ppHidden xmobarPP) windowSet screenId
                        , ppLayout = \t -> layoutOfScreenId windowSet screenId
                        , ppSort = fmap (. namedScratchpadFilterOutWorkspace) $ ppSort xmobarPP
                        }

xmobarColor' text foreground background bool =
    xmobarColor (if bool then foreground else background) (if bool then background else foreground) text

titleOfScreenId windowSet screenId =
    case L.find (\sc -> W.screen sc == S screenId) $ W.screens windowSet of
      Just sc -> case (W.stack $ W.workspace sc) >>= (\st -> Just $ W.focus st) of
                   Just w -> do
                       name <- getName w
                       return $ Just $ xmobarColor' (wrap " " (replicate 300 ' ') $ show name) black white $ (W.screen $ W.current windowSet) == W.screen sc
                   Nothing -> def
      Nothing -> titleOfScreenId windowSet 0 -- optimize

instance LM.LayoutModifier ResizableTall a where
--    modifyDescription
    modifyDescription (ResizableTall _nmaster _delta _frac _slaves) l = (show _nmaster) ++ "|" ++ (show _frac) ++ "|" ++ (show _slaves)

instance LM.LayoutModifier MultiCol a where
--    modifyDescription
    modifyDescription m l = show m

layoutOfScreenId windowSet screenId =
    case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
      Just sc -> do
        let layout = W.layout $ W.workspace sc
        description layout
--        case layout of
--          Layout l -> case l of
--                        ResizableTall _nmaster _delta _frac _slaves -> "yeah"
--                        _ -> description layout
--          _ -> description layout
      Nothing -> layoutOfScreenId windowSet 0 -- optimize

--currentOfScreenId windowSet screenId = if (W.screen(W.current windowSet) == S screenId) then xmobarColor "#4E4B42" "#D9D3BA" . wrap " " " " else wrap "" ""

--visibleOfScreenId windowSet screenId wid =
--    case L.find (\sc -> (W.screen sc) == S screenId) (W.visible windowSet) of
--      Just sc -> if (W.tag (W.workspace sc) == wid) then wrap "[" "]" wid else wrap "" "" wid
--      Nothing -> wrap "" "" wid

currentOfScreenId spaceForOther windowSet screenId =
    if W.screen(W.current windowSet) == S screenId then
        xmobarColor black white . wrap " " " "
    else if spaceForOther then
             wrap " " " "
         else
             wrap "" ""

visibleOfScreenId windowSet screenId familyId tag =
    showOnlyWorkspaceFor (\ws -> \sid -> \wid ->
                                 case L.find (\sc -> (W.screen sc) == S screenId) (W.visible windowSet) of
                                   Just sc -> if (W.tag $ W.workspace sc) == tag then xmobarColor black white $ wrap " " " " wid else wrap "" "" wid
                                   Nothing -> wrap "" "" wid) windowSet screenId familyId tag

showOnlyWorkspaceFor f windowSet screenId familyId = \w ->
                                  case L.elemIndex '_' w of
                                    Just i ->
                                        let
                                            (wsid, fid) = splitAt i w
                                        in
                                          if fid == ("_" ++ familyId) then
                                            f windowSet screenId wsid
                                          else ""
                                    Nothing -> f windowSet screenId w


screenIds windowSet xmobarScreenId = L.foldr (\a -> \b -> a ++ b) "" $ L.map (\sid ->
                                                                   xmobarColor' (wrap (if xmobarScreenId == sid then "[" else " ") (if xmobarScreenId == sid then "]" else " ") $ show $ sid + 1) black white $ (fromIntegral sid) == (W.screen $ W.current windowSet)) [0 .. L.length $ W.visible windowSet]
--                                                                   xmobarColor' (wrap " " " " $ xmobarColor' (show sid) black white $ xmobarScreenId == sid) black white $ (fromIntegral sid) == (W.screen $ W.current windowSet)) [0 .. L.length $ W.visible windowSet]

fallbackIfNoScreen f windowSet screenId =
    case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
      Just sc -> f windowSet screenId $ toFamilyId $ W.tag $ W.workspace $ sc
      Nothing -> f windowSet 0 (currentFamilyId windowSet)

viewToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.view . W.tag . W.workspace
    moveMouseToLastPosition

greedyViewToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.greedyView . W.tag . W.workspace
    moveMouseToLastPosition

shiftToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.shift . W.tag . W.workspace
    moveMouseToLastPosition

------------------------------------------------------------------------------------------
-- Workspaces
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- MousePosition
------------------------------------------------------------------------------------------
data MousePositionMap = MousePositionMap (M.Map ScreenId (Position, Position)) deriving Typeable
instance ExtensionClass MousePositionMap where
  initialValue = MousePositionMap M.empty
data MousePosition = MousePosition (Maybe (Position, Position)) deriving Typeable
instance ExtensionClass MousePosition where
  initialValue = MousePosition Nothing
logCurrentMouseLocation :: X ()
logCurrentMouseLocation = withWindowSet $ \ws -> do
  MousePosition maybeLastMousePosition <- XS.get
  xconf <- ask
  let maybeMousePos = mousePosition xconf
  ifX (maybeLastMousePosition /= maybeMousePos) $ do
    MousePositionMap lastMousePositions <- XS.get
    whenJust maybeMousePos $ \(x, y) -> do
      maybeScreen <- pointScreen x y
      whenJust (mfilter ((W.screen $ W.current ws) ==) $ fmap W.screen maybeScreen) $ \s -> do
        XS.put $ MousePositionMap $ M.insert s (x, y) lastMousePositions
        XS.put $ MousePosition $ maybeMousePos

moveMouseToLastPosition :: X ()
moveMouseToLastPosition =
  whenX (isNothing <$> dragging <$> get) $
    withWindowSet $ \ws -> do
      let rect = screenRect $ W.screenDetail $ W.current ws
      maybeMousePos <- mousePosition <$> ask
      case maybeMousePos of
        Just (x, y) -> ifX (not $ pointWithin x y rect) $ moveMouseToLastPosition' ws
        _ -> moveMouseToLastPosition' ws
moveMouseToLastPosition' ws = do
  MousePositionMap lastMousePositions <- XS.get
  let s = W.current ws
  let (x, y) = case M.lookup (W.screen s) lastMousePositions of
                 Just (x, y) -> (x, y)
                 Nothing -> do
                   let sd = W.screenDetail s
                   let rect = screenRect sd
                   let x = truncate $ (fromIntegral $ rect_x rect) + (fromIntegral $ rect_width rect) / 2
                   let y = truncate $ (fromIntegral $ rect_y rect) + (fromIntegral $ rect_height rect) / 2
                   (x, y)
  moveMouseTo x y
--        setMouseSpeedForScreen $ fromIntegral $ W.screen s

moveMouseTo x y = do
  rootw <- asks theRoot
  withDisplay $ \d ->
    io $ warpPointer d none rootw 0 0 0 0 (fromIntegral x) (fromIntegral y)

configureMouse = do
  moveMouseToLastPosition
  setMouseSpeedForScreen 0
  runProcessWithInputAndWait "sh" ["-c", "xinput --set-prop " ++ mouseDeviceId ++ " 'libinput Natural Scrolling Enabled' 1"] "" (seconds 1) -- Enable Natural scrooling
  runProcessWithInputAndWait "sh" ["-c", "xinput --set-prop " ++ mouseDeviceId ++ " 'libinput Click Method Enabled' 0 1"] "" (seconds 1) -- Right click on 2 fingure click

setMouseSpeedForScreen s = runProcessWithInputAndWait "sh" ["-c", "xinput --set-prop " ++ mouseDeviceId ++ " 'libinput Accel Speed' " ++ (mouseSpeed s)] "" (seconds 1)
mouseSpeed :: Int -> String
-- mouseSpeed n = ["0.791367", "1", "1"] !! n
mouseSpeed n = ["0.791367", "0.791367", "0.791367"] !! n
mouseDeviceId = "12"

------------------------------------------------------------------------------------------
-- MousePosition
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- GridSelect
------------------------------------------------------------------------------------------
hidpiGSConfig :: HasColorizer a => GSConfig a
hidpiGSConfig = (buildDefaultGSConfig nierColorizer) {
                  gs_cellheight = 80
                , gs_cellwidth = 800
                , gs_font = "xft:monospace-9:bold,Symbola-9:bold"
                , gs_navigate   = myNavNSearch
}

nierColorizer :: a-> Bool -> X (String, String)
nierColorizer a active =
  if active then
      return (black, white)
  else
      return (gray, black)

spawnAppSelected conf apps = gridselect conf apps >>= doForJust spawn

runActionSelected conf actions = gridselect conf actions >>= doForJust (\x -> x)

myNavNSearch :: TwoD a (Maybe a)
myNavNSearch = makeXEventhandler $ shadowWithKeymap navNSearchKeyMap navNSearchDefaultHandler
  where navNSearchKeyMap = M.fromList [
           ((0,xK_Escape)     , cancel)
          ,((controlMask,xK_g), cancel)
          ,((0,xK_Return)     , select)
          ,((0,xK_Left)       , move (-1,0) >> myNavNSearch)
          ,((controlMask, xK_b)       , move (-1,0) >> myNavNSearch)
          ,((0,xK_Right)      , move (1,0) >> myNavNSearch)
          ,((controlMask, xK_f)       , move (1,0) >> myNavNSearch)
          ,((0,xK_Down)       , move (0,1) >> myNavNSearch)
          ,((controlMask, xK_n)       , move (0,1) >> myNavNSearch)
          ,((0,xK_Up)         , move (0,-1) >> myNavNSearch)
          ,((controlMask, xK_p)       , move (0,-1) >> myNavNSearch)
          ,((controlMask, xK_a)       , move (-1,0) >> move (-1,0) >> move (-1,0) >> move (-1,0) >> move (-1,0) >> move (-1,0) >> move (-1,0) >> move (-1,0) >> myNavNSearch)
          ,((controlMask, xK_e)       , move (1,0) >> move (1,0) >> move (1,0) >> move (1,0) >> move (1,0) >> move (1,0) >> move (1,0) >> move (1,0) >> myNavNSearch)
          ,((0,xK_Tab)        , moveNext >> myNavNSearch)
          ,((shiftMask,xK_Tab), movePrev >> myNavNSearch)
          ,((0,xK_BackSpace), transformSearchString (\s -> if (s == "") then "" else init s) >> myNavNSearch)
          ]
        -- The navigation handler ignores unknown key symbols, therefore we const
        navNSearchDefaultHandler (_,s,mask) = do
          if mask == 0 then
            transformSearchString (++ s)
            >> myNavNSearch
          else
            myNavNSearch

scratchpadSelected :: GSConfig NamedScratchpad -> [NamedScratchpad] -> X()
scratchpadSelected config scratchpads = do
    scratchpadMaybe <- gridselect config (map (\s -> (name s, s)) scratchpads)
    myNamedScratchpadActionMaybe scratchpadMaybe

mySDConfig = def {
               activeColor = white
             , inactiveColor = black
             , urgentColor = "white"
             , activeTextColor = black
             , inactiveTextColor = white
             , urgentTextColor = "red"
             , activeBorderColor = white
             , inactiveBorderColor = black
             , urgentBorderColor = "pink"
             , decoHeight = 32
             , fontName = "xft:monospace-9:bold,Symbola-9:bold"
}

anyWorkspacePredicate :: WindowSet -> WindowSpace -> Bool
anyWorkspacePredicate windowset workspace = ("NSP" :: WorkspaceId) /= (W.tag workspace)
anyWorkspaceInCurrentWorkspaceFamilyPredicate :: WindowSet -> WindowSpace -> Bool
anyWorkspaceInCurrentWorkspaceFamilyPredicate windowset workspace = anyWorkspacePredicate windowset workspace && ((toFamilyId $ W.currentTag windowset) == (toFamilyId $ W.tag workspace))
visibleWorkspacesPredicate :: WindowSet -> WindowSpace -> Bool
visibleWorkspacesPredicate windowset workspace = anyWorkspacePredicate windowset workspace && ((W.tag workspace == W.currentTag windowset) || (L.elem (W.tag workspace) $ L.map (W.tag . W.workspace) $ W.visible windowset))

goToSelected' :: (WindowSet -> WindowSpace -> Bool) -> GSConfig Window -> X ()
goToSelected' =
    withSelectedWindow' $ \w -> do
      s <- gets windowset
      case W.findTag w s of
        Just tag -> windows $ (W.focusWindow w) . (W.greedyView tag)
        Nothing -> windows $ W.focusWindow w

shiftSelected' :: (WindowSet -> WindowSpace -> Bool) -> GSConfig Window -> X ()
shiftSelected' =
    withSelectedWindow' $ \w -> (windows $ \s -> W.shiftMaster $ W.focusWindow w $ W.shiftWin (W.currentTag s) w s)

-- | Like `gridSelect' but with the current windows and their titles as elements
gridselectWindow' :: (WindowSet -> WindowSpace -> Bool) -> GSConfig Window -> X (Maybe Window)
gridselectWindow' predicate gsconf = windowMap' predicate >>= gridselect gsconf

-- | Brings up a 2D grid of windows in the center of the screen, and one can
-- select a window with cursors keys. The selected window is then passed to
-- a callback function.
withSelectedWindow' :: (Window -> X ()) -> (WindowSet -> WindowSpace -> Bool) -> GSConfig Window -> X ()
withSelectedWindow' callback predicate conf = gridselectWindow' predicate conf >>= doForJust callback

windowMap' :: (WindowSet -> WindowSpace -> Bool) -> X [(String,Window)]
windowMap' predicate = do
    ws <- gets windowset
    wins <- mapM (keyValuePair ws) (foldr (++) [] $ map (W.integrate' . W.stack) $ filter (predicate ws) $ W.workspaces ws)
    return wins
 where keyValuePair ws w = flip (,) w `fmap` (decorateName' ws w)

decorateName' :: WindowSet -> Window -> X String
decorateName' ws w = do
  name <- getName' w
  clazz <- getClass' w
  workspace <- getWorkspace' w
  let workspaces = W.workspaces ws
  let focuses = L.map W.focus $ catMaybes $ L.map (W.stack) workspaces
  let classifier = if L.elem w focuses then "* " else "  "
  return ("[" ++ workspace ++ "] " ++ classifier ++ clazz ++ " : " ++ name)

getName' :: Window -> X String
getName' w = withDisplay $ \d -> do
    -- TODO, this code is ugly and convoluted -- clean it up
    let getIt = bracket getProp (xFree . tp_value) (copy)
        getProp = (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                      `E.catch` \(SomeException _) -> getTextProperty d w wM_NAME
        copy prop = fromMaybe "" . listToMaybe <$> wcTextPropertyToTextList d prop
    io $ getIt `E.catch` \(SomeException _) ->  (resName) `fmap` getClassHint d w

getClass' :: Window -> X String
getClass' w = withDisplay $ \d -> do
    -- TODO, this code is ugly and convoluted -- clean it up
    let getIt = bracket getProp (xFree . tp_value) (copy)
        getProp = getTextProperty d w wM_CLASS
        copy prop = fromMaybe "" . listToMaybe <$> wcTextPropertyToTextList d prop
    io $ getIt `E.catch` \(SomeException _) ->  (resName) `fmap` getClassHint d w

getWorkspace' :: Window -> X String
getWorkspace' w = withWindowSet $ \s -> do
                    case W.findTag w s of
                      Just tag -> return $ (toFamilyId tag) ++ "|" ++ (toWorkspaceId tag)
                      Nothing -> return ""
------------------------------------------------------------------------------------------
-- GridSelect
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- AllWindow
------------------------------------------------------------------------------------------
showAllWindow = windows $ \s -> W.greedyView "0_1" $ copyAllWindowTo "0_1" s

copyAllWindowTo ws s = foldr (\w -> \s' -> copyWindow w ws s') s $ W.allWindows s

-- selectWindow = windows $ \s
------------------------------------------------------------------------------------------
-- AllWindow
------------------------------------------------------------------------------------------

-----
viewScreen :: ScreenId -> X ()
viewScreen sid = screenWorkspace sid >>= doForJust (windows . W.view)

myrestart = withWindowSet $ myrestart' . W.screen . W.current
myrestart' sid = do
  if sid == 0 then
      spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
  else
      (viewScreen $ sid - 1) >> (myrestart' $ sid - 1)
------

screenInfo screenDetail = (show $ rect_width $ screenDetail) ++ "x" ++ (show $ rect_height $ screenDetail) ++ "+" ++ (show $ rect_x $ screenDetail) ++ "+" ++ (show $ rect_y $ screenDetail)

getEDID :: Rectangle -> X EDID
getEDID screenDetail = do
  edid <- runProcessWithInput "sh" ["-c", "xrandr --verbose | grep -A1000 ' connected .*" ++ screenInfo screenDetail ++ "' | grep -A1 EDID | head -n 2 | tail -n 1 | awk '{print $1}' | xargs echo -n"] ""
  return (edid :: EDID)
--getEDID screenDetail = runProcessWithInput "sh" ["-c", "xrandr --verbose | grep -A1000 ' connected " ++ (screenInfo screenDetail) ++ "' | grep -A1 EDID | head -n 2 | tail -n 1 | awk '{print $1}'"] ""
--getEDID screenDetail = runProcessWithInput "sh" ["-c", "echo ' connected " ++ (screenInfo screenDetail) ++ "'"] ""

-- debugEDID = withWindowSet $ \s -> do
--        io $ appendFile "/tmp/debug" $ "debugEDID" ++ (screenInfo $ screenRect $ W.screenDetail $ W.current s)
--        edid <- getEDID $ screenRect $ W.screenDetail $ W.current s
--        io $ appendFile "/tmp/debug" $ edid

debugEDID = getCurrentScreenEDIDMap

getCurrentScreenEDIDMap = withWindowSet $ \s -> do
                            screenEDIDList <- toScreenEDIDList ((W.current s) : (W.visible s))
                            io $ appendFile "/tmp/debug" $ (show screenEDIDList)

toScreenEDIDList [] = return []
toScreenEDIDList (screen:rest) = do
    let screenId = W.screen screen
    edid <- getEDID $ screenRect $ W.screenDetail $ screen
    screenEDIDList <- toScreenEDIDList rest
    return $ (screenId, edid) : screenEDIDList

type EDID = String
data ScreenEDIDMap = ScreenEDIDMap (M.Map ScreenId EDID) deriving Typeable
instance ExtensionClass ScreenEDIDMap where
  initialValue = ScreenEDIDMap M.empty

runProcessWithInput' :: MonadIO m => FilePath -> [String] -> String -> m String
runProcessWithInput' cmd args input = io $ do
    (pin, pout, perr, _) <- runInteractiveProcess (encodeString cmd)
                                            (map encodeString args) Nothing Nothing
    hPutStr pin input
    hClose pin
    output <- hGetContents pout
--    when (output == output) $ return ()
    hClose pout
    hClose perr
    -- no need to waitForProcess, we ignore SIGCHLD
    return output

--dunstEventHook e = return (All True) -- spawn "xdotool search --class Dunst | xargs xdotool windowraise" >> return (All True)

myrescreen :: [EDID] -> X ()
myrescreen priorityDisplayEDIDs = do
    xinesc <- (withDisplay getCleanedScreenInfo) :: X [Rectangle]
    spawn $ "echo 'xinesc: " ++ (show xinesc) ++ "' >> /tmp/xmonad.debug"

    edidToScreenRectangles <- (mapM (\screenRectangle -> do
        edid <- getEDID screenRectangle
        return (edid, screenRectangle)) xinesc) :: X [(EDID, Rectangle)]
    spawn $ "echo 'edidToScreenRectangles: " ++ (show edidToScreenRectangles) ++ "' >> /tmp/xmonad.debug"
    let edidToScreenRectanglesMap = M.fromList edidToScreenRectangles
    spawn $ "echo 'edidToScreenRectanglesMap: " ++ (show edidToScreenRectanglesMap) ++ "' >> /tmp/xmonad.debug"

    let prioritiedEDIDToScreenRectangles = L.foldr (++) [] $ L.map (\edid -> case M.lookup edid edidToScreenRectanglesMap of
                                                                       Just screenRectangle -> [(edid, screenRectangle)]
                                                                       Nothing -> []) priorityDisplayEDIDs
    spawn $ "echo 'prioritiedEDIDToScreenRectangles" ++ (show prioritiedEDIDToScreenRectangles) ++ "' >> /tmp/xmonad.debug"
    let sortedEDIDToScreenRectangles = prioritiedEDIDToScreenRectangles ++ (L.filter (\(edid, screenRectangle) -> not $ edid `elem` priorityDisplayEDIDs) edidToScreenRectangles)

    originalToCurrent <- originalScreenIdToCurrentScreenId priorityDisplayEDIDs
    spawn $ "echo 'originalToCurrent" ++ (show originalToCurrent) ++ "' >> /tmp/xmonad.debug"
    XS.put $ OriginalDisplayIdToCurrentScreenId $ originalToCurrent

    spawn $ "echo '" ++ (show sortedEDIDToScreenRectangles) ++ "' >> /tmp/xmonad.debug"
    windows $ \ws@(W.StackSet { W.current = v, W.visible = vs, W.hidden = hs }) ->
        let (xs, ys) = splitAt (length xinesc) $ map W.workspace (L.sortOn (W.screen) (v:vs)) ++ hs
            (a:as)   = zipWith3 W.Screen xs [0..] $ map SD $ map (snd) sortedEDIDToScreenRectangles
        in  ws { W.current = a
               , W.visible = as
               , W.hidden  = ys }

data OriginalDisplayIdToCurrentScreenId = OriginalDisplayIdToCurrentScreenId (M.Map Int Int) deriving Typeable
instance ExtensionClass OriginalDisplayIdToCurrentScreenId where
  initialValue = OriginalDisplayIdToCurrentScreenId M.empty

originalScreenIdToCurrentScreenId priorityDisplayEDIDs = do
    xinesc <- (withDisplay getCleanedScreenInfo) :: X [Rectangle]

    edidToOriginalScreenIds <- (mapM (\(i, screenRectangle) -> do
        edid <- getEDID screenRectangle
        return (edid, i)) $ indexed xinesc) :: X [(EDID, Int)]

    let edidToOriginalScreenIdsMap = M.fromList edidToOriginalScreenIds

    let prioritiedOriginalScreenIds = L.foldr (++) [] $ L.map (\edid -> case M.lookup edid edidToOriginalScreenIdsMap of
                                                                       Just i -> [i]
                                                                       Nothing -> []) priorityDisplayEDIDs
    let originalScreenIds = prioritiedOriginalScreenIds ++ (L.map (snd) $ L.filter (\(edid, i) -> not $ edid `elem` priorityDisplayEDIDs) edidToOriginalScreenIds)

    return $ M.fromList $ L.map (\(currentScreenId, originalScreenId) -> (originalScreenId, currentScreenId) ) $ indexed originalScreenIds


--sortedEDIDToScreenRectangles (priorityDisplayEDID:rest) edidToScreenRectanglesMap =
--    case M.lookup edid

indexed l = L.zip [0..(L.length l)] l

data LastScreenId = LastScreenId (Maybe ScreenId) deriving Typeable
instance ExtensionClass LastScreenId where
  initialValue = LastScreenId Nothing

checkAndHandleDisplayChange action =
    withWindowSet (\s -> do
      LastScreenId maybeScreenId <- XS.get
      case maybeScreenId of
        Just screenId ->
          ifX ((W.screen $ W.current s) /= screenId) $ do
            XS.put $ LastScreenId $ Just $ W.screen $ W.current s
            action
        Nothing ->
            (XS.put $ LastScreenId $ Just $ W.screen $ W.current s) >>
            action
    )

-- | The 'LayoutClass' instance for a 'ModifiedLayout' defines the
--   semantics of a 'LayoutModifier' applied to an underlying layout.
instance (LM.LayoutModifier l a, LayoutClass l a) => LayoutClass (MyModifiedLayout l) a where
    runLayout (W.Workspace i (MyModifiedLayout l) ms) r =
        do ((ws, ml'),mm')  <- LM.modifyLayoutWithUpdate l (W.Workspace i l ms) r
           (ws', mm'') <- LM.redoLayout (maybe l id mm') r ms ws
           let ml'' = case mm'' `mplus` mm' of
                        Just m' -> Just $ MyModifiedLayout $ maybe l id ml'
                        Nothing -> MyModifiedLayout `fmap` ml'
           return (ws', ml'')

    handleMessage (MyModifiedLayout l) mess =
        do mm' <- LM.handleMessOrMaybeModifyIt l mess
           ml' <- case mm' of
                  Just (Right mess') -> handleMessage l mess'
                  _ -> handleMessage l mess
           return $ case mm' of
                    Just (Left m') -> Just $ MyModifiedLayout $ maybe l id ml'
                    _ -> MyModifiedLayout `fmap` ml'
    description (MyModifiedLayout l) = LM.modifyDescription l l

-- | A 'ModifiedLayout' is simply a container for a layout modifier
--   combined with an underlying layout.  It is, of course, itself a
--   layout (i.e. an instance of 'LayoutClass').
data MyModifiedLayout l a = MyModifiedLayout (l a) deriving ( Read, Show )

-- N.B. I think there is a Haddock bug here; the Haddock output for
-- the above does not parenthesize (m a) and (l a), which is obviously
-- incorrect.


data AndroidLikeWindowView a = AndroidLikeWindowView {
      spaceRatio :: !Rational
    , spaceRatioInc :: !Rational
    , gapRatioWindows :: !Rational
    , deltaHeightRatio :: !Rational
    } deriving ( Read, Show )

instance LayoutClass AndroidLikeWindowView a where
    pureLayout l rect (W.Stack focus up down) = (layoutFocus l rect focus) ++ (layoutUp l rect up) ++ (layoutDown l rect down)

    pureMessage (AndroidLikeWindowView r i g d) m = case fromMessage m of
                                                    Just Shrink -> Just $ AndroidLikeWindowView (r + i) i g d
                                                    Just Expand -> Just $ AndroidLikeWindowView (r - i) i g d
                                                    _ -> Nothing

    description (AndroidLikeWindowView r i g d) = "AndroidLikeWindowView:" ++ (show r) ++ "," ++ (show g) ++ "," ++ (show d)

spaceWidth (AndroidLikeWindowView spaceRatio _ _ _) (Rectangle _ _ w _) = floor $ fromIntegral w * spaceRatio
spaceHeight (AndroidLikeWindowView spaceRatio _ _ _) (Rectangle _ _ _ h) = floor $ fromIntegral h * spaceRatio
gapWidth (AndroidLikeWindowView _ _ gapRatio _) (Rectangle _ _ w _) = floor $ fromIntegral w * gapRatio
deltaHeight (AndroidLikeWindowView _ _ _ deltaHeightRatio) (Rectangle _ _ _ h) = floor $ fromIntegral h * deltaHeightRatio

layoutFocus l@(AndroidLikeWindowView frac _ spaceFrac spaceHeightFrac) r@(Rectangle x y w h) focus =
    [(focus, Rectangle (x + fromIntegral sw) (y + fromIntegral sh) (w - sw * 2) (h - sh * 2))]
    where sw = spaceWidth l r
          sh = spaceHeight l r

layoutUp l r@(Rectangle x y w h) (left:xs) =
    [(left, Rectangle
              (x + fromIntegral sw - fromIntegral width - fromIntegral gw)
              (y + fromIntegral sh + fromIntegral dh)
              (width)
              (height))]
    where sw = spaceWidth l r
          sh = spaceHeight l r
          gw = gapWidth l r
          dh = deltaHeight l r
          width = w - sw * 2
          height = h - sh * 2 - (dh * 2)
layoutUp l r [] = []

layoutDown l r@(Rectangle x y w h) (right:xs) =
    [(right, Rectangle
               (x + fromIntegral sw + fromIntegral width + fromIntegral gw)
               (y + fromIntegral sh + fromIntegral dh)
               (width)
               (height))]
    where sw = spaceWidth l r
          sh = spaceHeight l r
          gw = gapWidth l r
          dh = deltaHeight l r
          width = w - sw * 2
          height = h - sh * 2 - (dh * 2)
layoutDown l r [] = []


data WindowViewableLayout windowViewLayout layout a = WindowViewableLayout (WindowViewState) (windowViewLayout a) (layout a) deriving ( Read, Show )

data WindowViewState = Normal | WindowView deriving ( Read, Show, Typeable )

data WindowViewMessage = View | Focus deriving ( Typeable )
instance Message WindowViewMessage

instance (LayoutClass l1 a, LayoutClass l2 a) => LayoutClass (WindowViewableLayout l1 l2) a where
    runLayout (W.Workspace i l@(WindowViewableLayout Normal l1' l2') ms) r = do
      (ws, ml) <- runLayout (W.Workspace i l2' ms) r
      case ml of
        Just nl -> return (ws, Just (WindowViewableLayout Normal l1' nl))
        Nothing -> return (ws, Nothing)

    runLayout (W.Workspace i l@(WindowViewableLayout WindowView l1' l2') ms) r = do
      (ws, ml) <- runLayout (W.Workspace i l1' ms) r
      case ml of
        Just nl -> return (ws, Just (WindowViewableLayout WindowView nl l2'))
        Nothing -> return (ws, Nothing)

    handleMessage l@(WindowViewableLayout state l1 l2) mess = do
      case fromMessage mess of
        Just View -> do
            handleMessage l2 $ SomeMessage Hide
            return $ Just $ WindowViewableLayout WindowView l1 l2
        Just Focus -> do
            handleMessage l1 $ SomeMessage Hide
            return $ Just $ WindowViewableLayout Normal l1 l2
        other -> delegateHandleMessage l mess

    description (WindowViewableLayout state l1 l2) = description l2
--      case state of
--        Normal -> "Normal" ++ (description l2)
--        WindowView -> "WindowView" ++ (description l1)

delegateHandleMessage (WindowViewableLayout Normal l1 l2) mess = do
  ml <- handleMessage l2 mess
  case ml of
    Just nl -> return $ Just (WindowViewableLayout Normal l1 nl)
    Nothing -> return Nothing
delegateHandleMessage (WindowViewableLayout WindowView l1 l2) mess = do
  ml <- handleMessage l1 mess
  case ml of
    Just nl -> return $ Just (WindowViewableLayout WindowView nl l2)
    Nothing -> return Nothing

instance ExtensionClass WindowViewState where
  initialValue = Normal

data CompositeTall layout a = CompositeTall {
      compositeTallCells :: [CompositeCell layout a],
      compositeTallRatioIncrement :: Rational,
      compositeTallLayoutTemplate :: layout a,
      compositeTallRestLayout :: layout a
  } deriving ( Read, Show )

data CompositeCell layout a = CompositeCell {
      compositeCellWindows :: Int,
      compositeCellRatio :: Rational,
      compositeCellLayout :: layout a
  } deriving ( Read, Show )

compositeTall ratioIncrement layoutTemplate = CompositeTall [CompositeCell 1 1 layoutTemplate] ratioIncrement layoutTemplate layoutTemplate

data NewCell = NewCellAtLeft | NewCellAtRight deriving ( Typeable )
instance Message NewCell

data DelegateMessage = DelegateMessage SomeMessage deriving ( Typeable )
instance Message DelegateMessage

data ResizeAnotherSide = ResizeAnotherSide Resize deriving ( Typeable )
instance Message ResizeAnotherSide

data ResetSize = ResetSize deriving ( Typeable )
instance Message ResetSize

instance (LayoutClass l a, Show a, Eq a) => LayoutClass (CompositeTall l) a where
    runLayout (W.Workspace tag layout stackMaybe) rect =
      case stackMaybe of
        Just stack -> do
--      let list = assignWindows layout windows
          let list = assignStack layout stack
          let rects = splitRect list rect
--          spawn $ "echo '" ++ (show rect) ++ "' >> /tmp/xmonad.debug.layout"
--          spawn $ "echo '" ++ (show (L.map (\(wins, r, _) -> (wins, r)) list)) ++ "' >> /tmp/xmonad.debug.layout"
--          spawn $ "echo '" ++ (show (L.map (\(wins, r, _) -> (wins, r)) rects)) ++ "' >> /tmp/xmonad.debug.layout"
          results <- applyLayouts rects tag stack
--          spawn $ "echo '" ++ (show results) ++ "' >> /tmp/xmonad.debug.layout"
          return (fst results,
            if L.all isNothing $ snd results then Nothing
            else Just $ replaceLayouts layout $ snd results)
--          return (L.map (\(wins, r, layout) -> (head wins, r)) rects, Nothing)
--      return ([(W.focus stack, rect)], Nothing)
        Nothing -> return ([], Nothing)

    handleMessage layout m = do
      maybeCells <- stackCells layout
      maybeLayouts <- stackLayouts layout
      let maybeNewLayout = msum [fmap (handleResize maybeCells) (fromMessage m)
                           ,fmap (handleResizeAnotherSide maybeCells maybeLayouts) (fromMessage m)
                           ,fmap handleResetSize (fromMessage m)
                           ,fmap (handleIncMasterN maybeCells) (fromMessage m)
                           ,fmap (handleNewCellMessage maybeLayouts) (fromMessage m)]
      case maybeNewLayout of
        Just newLayout ->
          case fromMessage m of
            Just ResetSize ->
              handleDelegateMessage newLayout maybeLayouts m >>= (return . Just . head . catMaybes . (:[maybeNewLayout]))
            _ -> return maybeNewLayout
        Nothing -> case fromMessage m of
            Just (DelegateMessage m) -> handleDelegateMessage layout maybeLayouts m
            _ -> handleDelegateMessage layout maybeLayouts m
      where
        handleResize (Just cells) Shrink = layout {compositeTallCells = replaceFocusedCell cells $ \c -> c {compositeCellRatio = max 0 $ compositeCellRatio c - compositeTallRatioIncrement layout}}
        handleResize (Just cells) Expand = layout {compositeTallCells = replaceFocusedCell cells $ \c -> c {compositeCellRatio = compositeCellRatio c + compositeTallRatioIncrement layout}}
        handleResizeAnotherSide (Just cells) (Just layouts) (ResizeAnotherSide resize) =
          if (L.null $ W.down layouts) || (L.null $ W.up cells) then
            handleResize (Just cells) $ reverseResize resize
          else do
            let up = head $ W.up cells
            let f = W.focus cells
            let ratioInc = compositeTallRatioIncrement layout
            let diff = case resize of
                         Expand -> min (compositeCellRatio up) ratioInc
                         Shrink -> min (compositeCellRatio f) ratioInc
            let replacedCells = cells {
                                  W.focus = f {
                                     compositeCellRatio = (case resize of
                                                             Expand -> (+)
                                                             Shrink -> (-)) (compositeCellRatio f) diff
                                  },
                                  W.up = (up {
                                     compositeCellRatio = (case resize of
                                                             Expand -> (-)
                                                             Shrink -> (+)) (compositeCellRatio up) diff
                                  }):(tail $ W.up cells)
                                }
            layout {
                 compositeTallCells = W.integrate replacedCells
            }
        handleResetSize ResetSize =
          layout {
            compositeTallCells = L.map (\c -> c {compositeCellRatio = 1}) $ compositeTallCells layout
          }
        handleIncMasterN (Just cells) (IncMasterN n) = do
          let newWins = (compositeCellWindows $ W.focus cells) + n
          if newWins <= 0 then
            layout {compositeTallCells = removeFocusedCell cells}
          else
            layout {compositeTallCells = replaceFocusedCell cells $ \c -> c { compositeCellWindows = newWins }}
        handleIncMasterN Nothing (IncMasterN n) = do
          if n > 0 then
            layout {compositeTallCells = [CompositeCell n 1 $ compositeTallLayoutTemplate layout]}
          else
            layout
        handleNewCellMessage (Just layouts) NewCellAtLeft = do
          let len = L.length $ W.up layouts
          let (ls,rs) = L.splitAt len $ compositeTallCells layout
          layout {compositeTallCells = ls ++ [CompositeCell 1 1 $ compositeTallLayoutTemplate layout] ++ rs}
        handleNewCellMessage (Just layouts) NewCellAtRight = do
          let len = L.length $ W.up layouts
          let (ls,rs) = L.splitAt (len+1) $ compositeTallCells layout
          layout {compositeTallCells = ls ++ [CompositeCell 1 1 $ compositeTallLayoutTemplate layout] ++ rs}
        handleDelegateMessage currentLayout (Just layouts) m = do
              ml <- handleMessage (W.focus layouts) m
              return (ml >>= (\l -> Just $ layouts {
                                      W.focus = Just l,
                                      W.up = L.map (\n -> Nothing) (W.up layouts),
                                      W.down = L.map (\n -> Nothing) (W.down layouts)
                                    })
                         >>= (Just . (replaceLayouts currentLayout) . W.integrate))
    description = show

data SimpleWide a = SimpleWide {
      simpleWideRatio :: [Rational],
      simpleWideRatioIncrement :: Rational,
      simpleWideRatioCurrentIndex :: Int,
      simpleWideRatioCurrentLength :: Int,
      simpleWideRectCache :: Maybe (Rectangle, [Rectangle])
} deriving ( Read, Show )

simpleWide ratioIncrement = SimpleWide [] ratioIncrement 0 0 Nothing

instance LayoutClass SimpleWide a where
    doLayout l@(SimpleWide ratio _ cidx clen mcache) rect stack = do
      let rs = ratio ++ (L.replicate (len - L.length ratio) 1)
      let mrs = case mcache of
                 Just (r, cache) -> if clen == len && rect == r then Just cache else Nothing
                 Nothing -> Nothing
      let rects = case mrs of
                 Just rs -> L.zip wins rs
                 Nothing -> do
                   let list = zipWith (\w r -> ([w], r, l)) wins rs
                   let rects = splitRect list $ mirrorRect rect
                   L.map (\(ws, r, l) -> (head ws, mirrorRect r)) rects
      return (rects,
        modifyLayout [
          (\layout -> if cidx == idx then Nothing
                      else Just $ layout {simpleWideRatioCurrentIndex = idx})
        , (\layout -> if rs == ratio then Nothing
                      else Just $ layout {simpleWideRatio = rs})
        , (\layout -> if clen == len then Nothing
                      else Just $ layout {simpleWideRatioCurrentLength = len})
        , (\layout -> if isNothing mrs then Just $ layout {simpleWideRectCache = Just (rect, L.map snd rects)}
                      else Nothing)
        ] l)
      where idx = L.length $ W.up stack
            wins = W.integrate stack
            len = L.length wins
    pureMessage (SimpleWide ratio ratioInc cidx clen _) m = do
        msum [fmap (handleResize l cidx) (fromMessage m)
             ,fmap handleResizeAnotherSide (fromMessage m)
             ,fmap handleResetSize (fromMessage m)]
      where l = SimpleWide ratio ratioInc cidx clen Nothing -- Evict cache
            rs layout = (simpleWideRatio layout) ++ (L.replicate (cidx - (L.length $ simpleWideRatio layout)) 1)
            handleResize layout i Shrink = do
              let (f, s) = splitAt (min (clen -2) i) $ rs layout
              if L.null s then layout
              else layout { simpleWideRatio = f ++ ((max 0 $ head s - ratioInc):(tail s)) }
            handleResize layout i Expand = do
              let (f, s) = splitAt (min (clen -2) i) $ rs layout
              if L.null s then layout
              else layout { simpleWideRatio = f ++ ((head s + ratioInc):(tail s)) }
            handleResizeAnotherSide (ResizeAnotherSide resize) =
                if (cidx == 0) || (cidx == clen - 1) then
                    handleResize l cidx $ reverseResize resize
                else do
                  let (f, s) = splitAt (min (clen -2) cidx-1) $ rs l
                  let up = head s
                  let down = head $ tail s
                  let diff = case resize of
                               Expand -> min up ratioInc
                               Shrink -> min down ratioInc
                  case resize of
                    Expand -> l { simpleWideRatio = f ++ [up - diff, down + diff] ++ (tail $ tail s) }
                    Shrink -> l { simpleWideRatio = f ++ [up + diff, down - diff] ++ (tail $ tail s) }
            handleResetSize ResetSize = l {simpleWideRatio = L.map (\r -> 1) ratio}

    description = show

reverseResize Shrink = Expand
reverseResize Expand = Shrink

modifyLayout fs defaultLayout = modifyLayout' fs Nothing defaultLayout
modifyLayout' (f:fs) maybeLayout defaultLayout = do
  let modifiedLayoutMaybe = f (case maybeLayout of
                                 Just l -> l
                                 Nothing -> defaultLayout)
  modifyLayout' fs (if isNothing modifiedLayoutMaybe then maybeLayout else modifiedLayoutMaybe) defaultLayout
modifyLayout' [] maybeLayout defaultLayout = maybeLayout

rebuildStack wins stack = do
  let (f, s) = span (W.focus stack /=) wins
  if L.null s then W.Stack (head wins) [] (L.tail wins)
  else W.Stack (head s) (L.reverse f) $ L.tail s
applyLayouts :: (LayoutClass layout a, Eq a) => [([a], Rectangle, layout a)] -> WorkspaceId -> W.Stack a -> X ([(a, Rectangle)], [Maybe (layout a)])
applyLayouts [] tag stack = return $ ([], [])
applyLayouts ((wins, rect, layout):list) tag stack = do
  (f, s) <- runLayout (W.Workspace tag layout $ Just $ rebuildStack wins stack) rect
  (fx, sx) <- applyLayouts list tag stack
  return ((f++fx), (s:sx))

stackCells :: CompositeTall layout a -> X (Maybe (W.Stack (CompositeCell layout a)))
stackCells layout = do
  let cellsLen = L.length $ compositeTallCells layout
  if cellsLen == 0 then return Nothing
  else
    withWindowSet $ \ws ->
      case W.stack $ W.workspace $ W.current ws of
        Just stack -> do
          let list = assignStack layout (stack {W.down = []})
          let (f, s) = splitAt ((min cellsLen $ L.length list) - 1) $ compositeTallCells layout
          if L.null s then return Nothing
          else do
            let focus = head s
            return $ Just $ W.Stack focus (reverse f) $ tail s
        Nothing -> do
          let cells = compositeTallCells layout
          return $ Just $ W.Stack (head cells) [] (tail cells)
stackLayouts :: CompositeTall layout a -> X (Maybe (W.Stack (layout a)))
stackLayouts layout = do
  let cells = compositeTallCells layout
  let layouts = (L.map compositeCellLayout cells) ++ [compositeTallRestLayout layout]
  withWindowSet $ \ws ->
    case W.stack $ W.workspace $ W.current ws of
      Just stack -> do
        let list = assignStack layout (stack {W.down = []})
        let (f, s) = splitAt ((min (L.length layouts) $ L.length list) - 1) $ layouts
        if L.null s then return Nothing
        else do
          let focus = head s
          return $ Just $ W.Stack focus (reverse f) $ tail s
      Nothing -> do
        return $ Just $ W.Stack (head layouts) [] (tail layouts)
replaceFocusedCell :: W.Stack (CompositeCell layout a) -> (CompositeCell layout a -> CompositeCell layout a) -> [CompositeCell layout a]
replaceFocusedCell stack@W.Stack{W.focus = fc} f = W.integrate $ stack {W.focus = f fc}
removeFocusedCell stack = (L.reverse $ W.up stack) ++ W.down stack

replaceLayouts layout@CompositeTall {
                     compositeTallCells = cells,
                     compositeTallRestLayout = layoutRest
               } layouts =
  layout {
    compositeTallCells = newCells,
    compositeTallRestLayout = case newLayoutRest of
                                Just l -> l
                                Nothing -> layoutRest
  }
  where newCells = L.zipWith (\c ml -> case ml of
                                  Just l -> c {compositeCellLayout = l}
                                  Nothing -> c) cells (layouts ++ repeat Nothing)
        newLayoutRest = head $ (L.drop (L.length cells) layouts) ++ [Nothing]

assignStack layout stack = assignWindows layout $ W.integrate stack
assignWindows :: CompositeTall layout a -> [b] -> [([b], Rational, layout a)]
assignWindows layout [] = []
assignWindows layout@(CompositeTall cells inc template rest) windows =
  if L.null cells then
    [(windows, -1, rest)]
  else do
    let CompositeCell wins ratio layout = head cells
    let (f, s) = splitAt wins windows
    (f, ratio, layout):(assignWindows (CompositeTall (tail cells) inc template rest) s)

splitRect :: [([a], Rational, layout a)] -> Rectangle -> [([a], Rectangle, layout a)]
splitRect list rect = splitRect' list rect (L.length list) 0
splitRect' :: [([a], Rational, layout a)] -> Rectangle -> Int -> Dimension -> [([a], Rectangle, layout a)]
splitRect' ((wins, ratio, layout):list) rect len currentWidth =
    if L.null list then
        [(wins, rect {rect_x = x, rect_width = mw - currentWidth}, layout)]
    else do
      let w = (truncate ((fromIntegral width) * ratio))
      if currentWidth + fromIntegral w >= fromIntegral mw then
          [(wins, (rect {rect_x = x, rect_width = mw - currentWidth}), layout)] ++ (splitRect' list rect len $ mw)
      else
          (wins, (rect {rect_x = x, rect_width = fromIntegral w}), layout):(splitRect' list rect len $ currentWidth + w)
    where x = rect_x rect + (fromIntegral currentWidth)
          mw = rect_width rect
          width = (fromIntegral $ mw) `div` len
splitRect' [] rect len cw = []

data TitleTransformer = TitleTransformer deriving (Read, Show, Eq, Typeable)

instance Transformer TitleTransformer Window where
    transform TitleTransformer x k = k (noFrillsDeco shrinkText mySDConfig x) (\(LM.ModifiedLayout _ x') -> x')

------------------------------------------------------------------------------------------
-- Terminal actions
------------------------------------------------------------------------------------------

terminalActionManageHook = onCenter'' 0.3 0.2

myTerminal = GnomeTerminal "xmonad.terminal.action"
selectWindowTerminalActionTemplate =
  (terminalActionTemplate "select.window" "~/.xmonad/terminal_actions/select_window.sh" $ onCenter'' 0.1 0.2)
  .| withFirstLine .|| words .|| head .|| read
selectActionTerminalActionTemplate =
  (terminalActionTemplate "select.action" "~/.xmonad/terminal_actions/select_action.sh" terminalActionManageHook)
  .| withFirstLine
dmenuRunTerminalAction =
  (terminalActionTemplate "dmenu.run" "~/.xmonad/terminal_actions/dmenu_run.sh" terminalActionManageHook)
  .| withFirstLine .>> spawn
openBrowserHistoryTerminalAction =
  (terminalActionTemplate "open.history" "~/.xmonad/terminal_actions/select_browser_history.sh" $ onCenter'' 0.1 0.2)
  .| withFirstLine .|| (\s -> "xdg-open '" ++ s ++ "'") .>> spawn
copyFromClipboardHistoryTerminalAction =
  (terminalActionTemplate "copy.from.clipboard.history" "~/.xmonad/terminal_actions/select_clipboard.sh" $ terminalActionManageHook) .>| ()
onePasswordTerminalAction =
  (terminalActionTemplate "one.password" "~/.xmonad/terminal_actions/one_password.sh" $ onCenter'' 0.1 0.2)
  .| withFirstLine .|| (\s -> "xdotool type '" ++ (T.unpack $ T.replace (T.pack "'") (T.pack "'\"'\"'") (T.pack s)) ++ "'") .>> spawn
openDashboardTerminalAction =
  (terminalActionTemplate "open.dashboard" "" terminalActionManageHook)
  .| withoutEmpty .|| (\outputs ->
                           if head outputs == "alt-enter" then
                               (False, tail outputs)
                           else
                               (True, outputs))
  .|| (\(appMode, urls) -> L.map (\url -> (if appMode then "vivaldi --app='" else "xdg-open '") ++ url ++ "'") urls)
  .|| L.map spawn
  .>> L.foldr (>>) (return ())

myTerminalActions = [
   (terminalActionTemplate "open.intellij" "~/.xmonad/terminal_actions/open_intellij.sh" terminalActionManageHook)
   .| withFirstLine .|| ((intellijCommand ++ " ") ++) .>> spawn
  , dmenuRunTerminalAction
  , openBrowserHistoryTerminalAction
  , copyFromClipboardHistoryTerminalAction
  , onePasswordTerminalAction
  , selectWindowTerminalActionTemplate .>| ()
  , selectActionTerminalActionTemplate .>| ()
  , openDashboardTerminalAction]

myTerminalActionHandleEventHook = keepWindowSizeHandleEventHook $ L.foldr (<||>) (return False) $ L.map (terminalQuery myTerminal) myTerminalActions

openIntelliJTerminalAction = do
  runNamedTerminalAction myTerminal myTerminalActions "open.intellij"

data BrowserHistoryTerminalActionState = BrowserHistoryTerminalActionState Int deriving (Typeable)
instance ExtensionClass BrowserHistoryTerminalActionState where
  initialValue = BrowserHistoryTerminalActionState 0
runOpenBrowserHistoryTerminalAction = do
  let sort = ["often", "recent"]
  runCyclicTerminalAction myTerminal "open.browser.history" $
                          L.map (openBrowserHistoryTerminalAction .<) sort

runCopyFromClipboardHistoryTerminalAction = do
  runNamedTerminalAction myTerminal myTerminalActions "copy.from.clipboard.history"

runOnePasswordTerminalAction = do
  runNamedTerminalAction myTerminal myTerminalActions "one.password"

data SelectedWindowTerminalActionState = SelectedWindowTerminalActionState String Int deriving (Typeable)
instance ExtensionClass SelectedWindowTerminalActionState where
  initialValue = SelectedWindowTerminalActionState "" 0

smartGreedyViewSelectedWindowTerminalAction =
  runSelectedWindowTerminalAction "smart.greedy.view" smartGreedyViewWindow

greedyViewSelectedWindowTerminalAction =
  runSelectedWindowTerminalAction "greedy.view" greedyViewWindow

shiftSelectedWindowTerminalAction =
  runSelectedWindowTerminalAction "shift" $ \w -> windows $ \s -> W.shiftMaster $ W.focusWindow w $ W.shiftWin (W.currentTag s) w s

runSelectedWindowTerminalAction myname handler predicates =
    runCyclicTerminalAction myTerminal myname $
                            L.map (\(header, predicate) -> selectWindowTerminalAction header predicate .>> handler) predicates

selectWindowTerminalAction header predicate  =
  selectWindowTerminalActionTemplate .<. ((windowMap' predicate) >>= (\l -> return $ ("0 " ++ header):(L.map (\(s, w) -> (show w) ++ " " ++ s) l)))

smartGreedyViewWindow = greedyViewWindow' True
greedyViewWindow = greedyViewWindow' False

greedyViewWindow' screenAware w  = do
  s <- gets windowset
  case W.findTag w s of
    Just tag -> do
      ifX screenAware $ do
        let fid = toFamilyId tag
        caseMaybeJust (L.find ((fid ==) . toFamilyId . W.tag . W.workspace) $ W.visible s) $ viewScreen . W.screen
      windows $ (W.focusWindow w) . (W.greedyView tag)
    Nothing -> windows $ W.focusWindow w

spawnAppSelectedTerminalAction' apps =
    runCyclicTerminalAction myTerminal "spawn.app" [mySpawnSelectedAppTerminalAction apps, dmenuRunTerminalAction]

myRunSelectedXTerminalAction = runSelectedXTerminalAction myTerminal selectActionTerminalActionTemplate
mySpawnSelectedAppTerminalAction = spawnSelectedAppTerminalAction selectActionTerminalActionTemplate
mySelectedXTerminalAction = selectedXTerminalAction selectActionTerminalActionTemplate

runDmenuRunTerminalAction = runTerminalAction myTerminal dmenuRunTerminalAction

runOpenDashboardTerminalAction = do
  runCyclicTerminalAction myTerminal "open.dashboard" $ L.map (\script -> openDashboardTerminalAction { actionScript = script }) [
                                  "~/.xmonad/terminal_actions/open_cluster_dashboard.sh",
                                  "~/.xmonad/terminal_actions/open_host_dashboard.sh",
                                  "~/.xmonad/terminal_actions/open_I.sh",
                                  "~/.xmonad/terminal_actions/open_I_K.sh"]
runOpenITerminalAction = do
  runCyclicTerminalAction myTerminal "open.I" $ L.map (\script -> openDashboardTerminalAction { actionScript = script }) [
                                  "~/.xmonad/terminal_actions/open_I.sh",
                                  "~/.xmonad/terminal_actions/open_I_K.sh"]

------------------------------------------------------------------------------------------
-- Terminal actions
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- Split screen
------------------------------------------------------------------------------------------
data ScreenLayout a = forall l. (LayoutClass l a, Read (l a)) => ScreenLayout (l a)

instance LayoutClass ScreenLayout ScreenId where
    runLayout (W.Workspace i (ScreenLayout l) ms) r = fmap (fmap ScreenLayout) `fmap` runLayout (W.Workspace i l ms) r
    doLayout (ScreenLayout l) r s  = fmap (fmap ScreenLayout) `fmap` doLayout l r s
    emptyLayout (ScreenLayout l) r = fmap (fmap ScreenLayout) `fmap` emptyLayout l r
    handleMessage (ScreenLayout l) = fmap (fmap ScreenLayout) . handleMessage l
    description (ScreenLayout l)   = description l

instance Show (ScreenLayout a) where show (ScreenLayout l) = show l

data VirtualScreen = VirtualScreen {
  rootSid :: ScreenId,
  originalRect :: Rectangle,
  screenLayout :: ScreenLayout ScreenId,
  screenStack :: W.Stack ScreenId
} deriving (Typeable, Show)

data VirtualScreens = VirtualScreens [VirtualScreen] deriving (Typeable, Show)
instance ExtensionClass VirtualScreens where
  initialValue = VirtualScreens []

resetVirtualScreens = XS.put $ VirtualScreens []

findVirtualScreen :: VirtualScreens -> ScreenId -> Maybe VirtualScreen
findVirtualScreen (VirtualScreens virtualScreens) sid =
  L.find (L.elem sid . W.integrate . screenStack) virtualScreens

newVirtualScreen screen layout =
  VirtualScreen {
    rootSid = sid,
    originalRect = screenRect $ W.screenDetail screen,
    screenLayout = ScreenLayout layout,
    screenStack = W.Stack {
                    W.focus = sid,
                    W.up = [],
                    W.down = []
                  }
  }
  where sid = W.screen screen

insertScreenStack :: VirtualScreen -> ScreenId -> VirtualScreen
insertScreenStack vs@VirtualScreen{screenStack = ss} sid =
  vs {
    screenStack = ss {
                    W.down = down ++ [sid]
                  }
  }
  where down = W.down ss

replaceVirtualScreen :: VirtualScreens -> VirtualScreen -> VirtualScreens
replaceVirtualScreen (VirtualScreens vss) vs =
  VirtualScreens $ vs:(L.filter ((rootSid vs /=) . rootSid) vss)

resetVirtualScreen :: X()
resetVirtualScreen = do
  virtualScreens <- XS.get
  withWindowSet $ \ws -> do
    let current = W.current ws
    let sid = W.screen current
    case findVirtualScreen virtualScreens $ sid of
      Just vs -> do
        let sids = L.delete sid $ W.integrate $ screenStack vs
        let newVisible = L.filter (\e -> L.notElem (W.screen e) sids) $ W.visible ws
        let workspaces = L.map W.workspace $ L.filter (\e -> L.elem (W.screen e) sids) $ W.visible ws
        XS.put $ replaceVirtualScreen virtualScreens $ vs {
                                                         screenStack = W.Stack { W.focus = rootSid vs, W.up = [], W.down = [] }
                                                       }
        windows $ \_ -> ws {
          W.current = current {
                        W.screen = rootSid vs,
                        W.screenDetail = SD $ originalRect vs
                      },
          W.visible = newVisible,
          W.hidden = (W.hidden ws) ++ workspaces
        }
      Nothing -> return ()


createVirtualScreen :: (LayoutClass l ScreenId, Read (l ScreenId)) => (l ScreenId) -> X ()
createVirtualScreen defaultLayout = do
  virtualScreens <- XS.get
  withWindowSet $ \ws -> whenX (return $ not $ L.null $ W.hidden ws) $ do
    let current = W.current ws
    let visible = W.visible ws
    let allScreen = current:visible
    let vs = case findVirtualScreen virtualScreens $ W.screen current of
               Just vs -> vs
               Nothing -> newVirtualScreen current defaultLayout

    let newSid = S $ (L.maximum $ L.map (\(S id) -> id) $ L.map (W.screen) allScreen) + 1
    let nvs = insertScreenStack vs newSid

    (newRects, newLayoutMaybe) <- runLayout (W.Workspace {
                 W.tag = show $ rootSid nvs,
                 W.layout = screenLayout nvs,
                 W.stack = Just $ screenStack nvs}) (originalRect nvs)

    XS.put $ replaceVirtualScreen virtualScreens $ case newLayoutMaybe of
                                                     Just newLayout -> nvs {screenLayout = newLayout}
                                                     Nothing -> nvs

    let nextWS = head $ W.hidden ws
    let newHidden = tail $ W.hidden ws

    let newCurrent = replaceScreenRect newRects $ W.Screen {
                       W.workspace = nextWS,
                       W.screen = newSid,
                       W.screenDetail = SD $ originalRect nvs
                     }
    let newVisible = L.map (replaceScreenRect newRects) allScreen

    windows $ \_ -> ws {
      W.current = newCurrent,
      W.visible = newVisible,
      W.hidden = newHidden
    }

replaceScreenRect rects screen =
  case findScreenRect rects $ W.screen screen of
    Just rect -> screen {W.screenDetail = SD rect}
    Nothing -> screen

findScreenRect :: [(ScreenId, Rectangle)] -> ScreenId -> Maybe Rectangle
findScreenRect rects sid = fmap snd $ L.find ((sid ==) . fst) rects

sendScreenMessage msg = do
  virtualScreens <- XS.get
  withWindowSet $ \ws -> do
    let current = W.current ws
    spawn $ "echo '" ++ (show virtualScreens) ++ "' >> /tmp/xmonad.debug.screen"
    case findVirtualScreen virtualScreens $ W.screen current of
      Just vs -> do
        let stack = screenStack vs
        let (up, focus:down) = span (W.screen current /=) $ W.integrate stack
        let newStack = W.Stack {W.focus = focus, W.up = L.reverse up, W.down = down}
        (_, layoutMaybe) <- runLayout (W.Workspace {
                 W.tag = show $ rootSid vs,
                 W.layout = screenLayout vs,
                 W.stack = Just $ newStack}) (originalRect vs)
        spawn $ "echo '" ++ (show layoutMaybe) ++ "' >> /tmp/xmonad.debug.screen"
        let layout = case layoutMaybe of
                       Just l -> l
                       Nothing -> screenLayout vs
        newLayoutMaybe <- handleMessage layout (SomeMessage msg) `catchX` return Nothing
        whenJust newLayoutMaybe $ \l -> do
          (rect, _) <- runLayout (W.Workspace {
                 W.tag = show $ rootSid vs,
                 W.layout = l,
                 W.stack = Just $ newStack}) (originalRect vs)
          XS.put $ replaceVirtualScreen virtualScreens $ vs {screenLayout = l}
          let visible = W.visible ws
          windows $ \_ -> ws {
            W.current = replaceScreenRect rect current,
            W.visible = L.map (replaceScreenRect rect) visible
          }
      Nothing -> return ()

rootSids vss ws = L.sort $ L.nub $ L.map (findRootSid vss) $ W.screens ws

currentRootSid vss ws = findRootSid vss $ W.current ws

findRootSid vss screen = case findVirtualScreen vss sid of
                        Just vs -> rootSid vs
                        Nothing -> sid
  where sid = W.screen screen

nextRootScreen = focusRootScreen id
prevRootScreen = focusRootScreen L.reverse
focusRootScreen f = do
  virtualScreens <- XS.get
  windows $ \ws -> do
    let rootSids' = rootSids virtualScreens ws
    let currentRoot = currentRootSid virtualScreens ws
    let nextSid = head $ tail $ dropWhile (currentRoot /=) $ cycle $ f rootSids'
    let (up, focus:down) = L.span ((nextSid /=) . W.screen) $ W.screens ws
    ws {
      W.current = focus,
      W.visible = up ++ down
    }

nextVirtualScreen = focusVirtualScreen id
prevVirtualScreen = focusVirtualScreen L.reverse
focusVirtualScreen f = do
  virtualScreens <- XS.get
  withWindowSet $ \ws -> do
    let sid = W.screen $ W.current ws
    whenJust (findVirtualScreen virtualScreens sid) $ \vs ->
      whenJust (focusIt sid $ screenStack vs) $ \s ->
        XS.put $ replaceVirtualScreen virtualScreens $ vs {screenStack = s}
    windows $ \ws -> do
      let rootSids' = rootSids virtualScreens ws
      let currentRoot = currentRootSid virtualScreens ws
      let nextRootSid = head $ tail $ dropWhile (currentRoot /=) $ cycle $ f rootSids'
      let focusSid = fromMaybe nextRootSid $ W.focus <$> screenStack <$> findVirtualScreen virtualScreens nextRootSid
      let (up, focus:down) = L.span ((focusSid /=) . W.screen) $ W.screens ws
      ws {
        W.current = focus,
        W.visible = up ++ down
      }

shiftNextRootScreen = shiftRootScreen id
shiftPrevRootScreen = shiftRootScreen L.reverse
shiftRootScreen f = do
  virtualScreens <- XS.get
  windows $ \ws -> do
    let rootSids' = rootSids virtualScreens ws
    let currentRoot = currentRootSid virtualScreens ws
    let prevSid = head $ tail $ dropWhile (currentRoot /=) $ cycle $ f rootSids'
    case L.find ((prevSid ==) . W.screen) $ W.screens ws of
      Just s -> W.shift (W.tag $ W.workspace s) ws
      Nothing -> ws

focusIt a stack = do
  let (up, focusDown) = L.span (a /=) $ W.integrate stack
  case focusDown of
    f:d -> Just $ W.Stack {W.focus = f, W.up = L.reverse up, W.down = d}
    _ -> Nothing
nextChildScreen = focusChildScreen W.focusDown'
prevChildScreen = focusChildScreen W.focusUp'
focusChildScreen f = do
  virtualScreens <- XS.get
  windows $ \ws -> do
    let currentSid = W.screen $ W.current ws
    case findVirtualScreen virtualScreens currentSid of
      Just vs -> do
        case focusIt currentSid $ screenStack vs of
          Just stack -> do
            let newStack = f stack
            let focus = W.focus newStack
            if focus == currentSid then ws
            else case L.find ((focus ==) . W.screen) $ W.visible ws of
                   Just s -> ws {
                               W.current = s,
                               W.visible = (W.current ws):(L.filter ((focus /=) . W.screen) $ W.visible ws)
                             }
                   Nothing -> ws
          _ -> ws
      Nothing -> ws
------------------------------------------------------------------------------------------
-- Split screen
------------------------------------------------------------------------------------------
