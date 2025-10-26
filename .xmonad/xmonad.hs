{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
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
import Data.Time.Clock
import Data.Time.Calendar

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
import XMonad.Actions.MetaMeta
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

import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import qualified XMonad.Layout.LayoutModifier as LM
--import XMonad.Layout.LayoutScreens
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
import XMonad.Layout.AndroidLikeWindowView
import XMonad.Layout.WindowViewableLayout
import XMonad.Layout.CompositeTall

import XMonad.Util.Run(spawnPipe, runProcessWithInput, runProcessWithInputAndWait, seconds)
import XMonad.Util.DunstSupport
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad2
import XMonad.Util.MyNamedScratchpad
import XMonad.Util.NamedWindows
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.HandleEventHooks
import XMonad.Util.ManageHookUtils
import XMonad.Util.WorkspaceHistory
import XMonad.Util.SwitchableLogHook
import XMonad.Util.HandleScreenChange

import XMonad.Util.Performance
import XMonad.Layout.CachedLayout

import XMonad.Util.MyUtils

import XMonad.Util.AdvancedMouse
import XMonad.Util.VirtualMouse
import XMonad.Util.PhysicalScreen
import XMonad.Util.VirtualScreen
import XMonad.Util.WorkspaceFamily

import XMonad.Util.DocksSupport
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, ToggleStruts(..))

import XMonad.Actions.FloatAwareFocus

import XMonad.Actions.DrawShape

import XMonad.Conf.MyConf
import XMonad.Conf.MyTerminalAction
import XMonad.Conf.MyTerminalAction.Fallback
import XMonad.Conf.MyHook

xmobarLogHook xmprocs = withWindowSet (\s ->
    L.foldl (>>) def (map (\(i, xmproc) -> do
--      originalScreenIdToCurrentScreenIdMap <- originalScreenIdToCurrentScreenId priorityDisplayEDIDs
        j <- (\(OriginalDisplayIdToCurrentScreenId idToId) -> fromMaybe i $ M.lookup i idToId) <$> XS.get
        vss <- getVirtualScreens
        let screenList = toScreenList s vss j
        let virtualScreenList = toVirtualScreenList s vss j
        let header = if virtualScreenList == "" then screenList else screenList ++ " | " ++ virtualScreenList
        let S virtualScreenId = maybe (S j) (\vs -> W.focus $ screenStack vs) $ findVirtualScreen vss $ S j
        dynamicLogWithPP (multiScreenXMobarPP s virtualScreenId header xmproc)) (L.zip [0..(L.length xmprocs)] xmprocs)))

toScreenList ws vss xmobarScreenId = do
  let sidCurrentActive = W.screen $ W.current ws
  let S sidCurrentActiveRoot = maybe sidCurrentActive (\vs -> rootSid vs) (findVirtualScreen vss sidCurrentActive)
  let sids = L.sort $ L.map (\(S sid) -> sid) $ L.map W.screen $ W.screens ws
  let screenDesc = L.map (\sid -> do
                            let screenStr = toScreen sid sidCurrentActiveRoot xmobarScreenId
                            let vsMaybe = findVirtualScreen vss $ S sid
                            maybe (screenStr) (\vs -> if rootSid vs == S sid then screenStr else "") vsMaybe
                         ) sids
  L.foldr (++) ("") screenDesc

toScreen sid currentActive xmobarScreenId =
    xmobarColor'
    (wrap (if xmobarScreenId == sid then "[" else " ") (if xmobarScreenId == sid then "]" else " ") $
          show $ sid + 1)
    black white $ sid == currentActive


toVirtualScreenList ws vss xmobarScreenId = do
  let S sidCurrentActive = W.screen $ W.current ws
  case findVirtualScreen vss $ S xmobarScreenId of
    Just vs -> do
      let sids = L.map (\(S sid) -> sid) $ W.integrate $ screenStack vs
      L.foldr (++) ("") $ L.map (toVirtualScreen sidCurrentActive) sids
    Nothing -> ""
toVirtualScreen currentActive sid =
    xmobarColor'
    (wrap " " " " $ show $ sid + 1)
    black white $ sid == currentActive

--value_mask :: !CULong = (bit 2) (.|.) (bit 3)

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
  , ((mod4Mask, xK_r), myrefresh)
  -- Screenshot
  , ((0, xK_Print), spawn "sh ~/.xmonad/screenshot.sh")
--         , ((controlMask, xK_Print), spawn "gnome-screenshot -c")
  , ((mod4Mask, xK_s), spawn "sh ~/.xmonad/screenshot.sh")
  , ((mod4Mask .|. shiftMask, xK_s), spawn "sh ~/.xmonad/screenshot.sh -a")

  -- Performance
  -- , ((mod4Mask, xK_a), do
  --      durations <- getDurations
  --      spawn $ "echo '" ++ (show durations) ++ "' >> /tmp/xmonad.perf")
  ]

myrefresh = withWindowSet $ \ws -> do
  let sid = W.screen $ W.current ws
  viewScreen 0
  refresh
  rePhysicalScreen priorityDisplayEDIDs
  docksStartupHook
  resetVirtualScreens
  initializeScreenMouses
  viewScreen sid

dunstKeys = [
    ((mod4Mask, xK_slash), spawn "dunstctl close")
  , ((mod4Mask .|. shiftMask .|. controlMask, xK_slash), spawn "dunstctl close-all")
  , ((mod4Mask .|. shiftMask, xK_slash), spawn "dunstctl history-pop")
  , ((mod4Mask .|. controlMask, xK_slash), spawn "dunstctl context")
  , ((mod4Mask .|. altMask, xK_slash), spawn "dunstctl set-paused toggle")
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
    ((mod4Mask, xK_t), windows floatFocusDown)
  , ((mod4Mask .|. shiftMask, xK_t), windows floatFocusUp)
  , ((mod4Mask .|. controlMask, xK_t), withFocused $ windows . W.sink) -- %! Push window back into tiling
  ]

screenKeys = L.concat $ [
    bindKeys [(meta .|. alt, xK_p)
             ,(meta .|. shft, xK_space)] $ prevVirtualScreen
  , bindKeys [(meta .|. alt, xK_n)
             ,(meta, xK_space)] $ nextVirtualScreen
  , bindKey (meta .|. shft .|. alt) xK_p $ shiftPrevRootScreen >> prevVirtualScreen
  , bindKey (meta .|. shft .|. alt) xK_n $ shiftNextRootScreen >> nextVirtualScreen
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
    ((mod4Mask, xK_x), createVirtualScreen' (myLayoutForVirtualScreen ||| (Mirror myLayoutForVirtualScreen)) $ selectUnusedFamilyWorkspace)
  , ((mod4Mask .|. controlMask, xK_x), removeVirtualScreen)
  , ((mod4Mask .|. shiftMask, xK_x), resetVirtualScreen)
  , ((mod4Mask .|. mod1Mask, xK_d), sendScreenMessage NextLayout)
  , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_comma    ), sendScreenMessageToCompositeTall (IncMasterN 1))
  , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_period), sendScreenMessageToCompositeTall (IncMasterN (-1)))
  , ((mod4Mask .|. mod1Mask .|. controlMask, xK_comma     ), sendScreenMessageToCompositeTall NewCellAtLeft)
  , ((mod4Mask .|. mod1Mask .|. controlMask, xK_period ), sendScreenMessageToCompositeTall NewCellAtRight)
  , ((mod4Mask .|. mod1Mask, xK_j), sendScreenMessageToCompositeTall Shrink)
  , ((mod4Mask .|. mod1Mask, xK_l), sendScreenMessageToCompositeTall Expand)
  , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_j), sendScreenMessageToCompositeTall $ ResizeAnotherSide Expand)
  , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_l), sendScreenMessageToCompositeTall $ ResizeAnotherSide Shrink)
  , ((mod4Mask .|. mod1Mask, xK_i), sendScreenMessageToCompositeTall $ DelegateMessage $ SomeMessage Shrink)
  , ((mod4Mask .|. mod1Mask, xK_k), sendScreenMessageToCompositeTall $ DelegateMessage $ SomeMessage Expand)
  , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_i), sendScreenMessageToCompositeTall $ DelegateMessage $ SomeMessage $ ResizeAnotherSide Expand)
  , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_k), sendScreenMessageToCompositeTall $ DelegateMessage $ SomeMessage $ ResizeAnotherSide Shrink)
  , ((mod4Mask .|. mod1Mask, xK_b), prevChildScreen)
  , ((mod4Mask .|. mod1Mask, xK_f), nextChildScreen)
  ]

sendScreenMessageToCompositeTall msg = do
  vs <- currentVirtualScreen
  caseMaybeJust vs $ \it -> do
    let pos = L.length $ W.up $ screenStack it
    sendScreenMessage $ CompositeTallMessage {message = SomeMessage $ msg, messageAtWindow = pos}

layoutKeys = [
  -- Layout
    ((mod4Mask, xK_d), sendMessage NextLayout)
  , ((mod4Mask .|. shiftMask, xK_comma    ), sendMessage (IncMasterN 1))
  , ((mod4Mask .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))
  , ((mod4Mask .|. controlMask, xK_comma     ), sendMessage NewCellAtLeft)
  , ((mod4Mask .|. controlMask, xK_period ), sendMessage NewCellAtRight)

  -- Struts
  , ((mod4Mask, xK_h), docksOnBottom >> (sendMessage ToggleStruts))
  , ((mod4Mask .|. shiftMask, xK_h), sendMessage $ XMonad.Layout.MyMultiToggle.Toggle TitleTransformer)

  -- Window view
  , ((mod4Mask, xK_v), quitWindowView $ sendMessage ToggleLayout)
  , ((mod4Mask .|. controlMask, xK_v), sendMessage $ DelegateMessage $ SomeMessage $ ToggleLayout)
  , ((mod4Mask .|. shiftMask, xK_v), startWindowView)
  ]

sizingKeys = [
    ((meta,          xK_j), whenX (xnot $ locateFloat $ onLeft)
                              $ sendMessage Shrink)
  , ((meta,          xK_l), whenX (xnot $ locateFloat $ onRight)
                              $ sendMessage Expand)
  , ((meta,          xK_i), whenX (xnot $ locateFloat $ onTop)
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

  , ((mod4Mask, xK_a), myNamedScratchpadAction "ai")

  , ((mod4Mask .|. controlMask, xK_F7), toggleScrachpadAction $ L.reverse myScratchpads)
  , ((mod4Mask .|. controlMask, xK_F8), withoutLogHook $ showOrHideScratchpads myScratchpads True)
  , ((mod4Mask .|. controlMask .|. shiftMask, xK_F8), withoutLogHook $ showOrHideScratchpads myScratchpads False)
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
  , ((mod4Mask              , xK_F7), spawn "intel-pstate-utils-update-freq min up; killall -SIGUSR1 xmobar_metrics_daemon")
  , ((mod4Mask .|. shiftMask, xK_F7), spawn "intel-pstate-utils-update-freq min down; killall -SIGUSR1 xmobar_metrics_daemon")
  , ((mod4Mask              , xK_F8), spawn "intel-pstate-utils-update-freq max up; killall -SIGUSR1 xmobar_metrics_daemon")
  , ((mod4Mask .|. shiftMask, xK_F8), spawn "intel-pstate-utils-update-freq max down; killall -SIGUSR1 xmobar_metrics_daemon")
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

  --
  , ((meta, xK_z), drawShapeOnMouse $ DrawShape OutlinedRectangle 5 red_rgb)
  , ((meta .|. shft, xK_z), drawShapeOnMouse $ DrawShape FilledRectangle 5 red_rgb)
  , ((meta .|. ctrl, xK_z), drawShapeOnMouse $ DrawShape LongestStraightLine 5 red_rgb)
  , ((meta .|. alt, xK_z), removeLatestDrawnShape)
  ]

workspaceHistoryKeys = [
    ((mod4Mask, xK_comma ), undoWorkspaceHistory)
  , ((mod4Mask, xK_period), redoWorkspaceHistory)
  ]

main = do
    homeDirectory <- liftIO getHomeDirectory

    -- Display
    runProcessWithInputAndWait "sh" ["-c", "bash '" ++ homeDirectory ++ "/.xmonad/auto_detect_display.sh' >> auto_detect_display_debug"] "" (seconds 1)

    -- Keyboard and Mouse
    spawn "xhost +SI:localuser:root; sleep 1; sudo xkeysnail --watch -q ~/config.py & sleep 3; xset r rate 210 70; xset q >> /tmp/xset.debug"
    --spawn "xhost +SI:localuser:root; sleep 1; sudo xkeysnail --watch -q ~/config.py"
    spawn "sudo libinput-gestures"

    -- Desktop
    spawn "sleep 3; feh --bg-fill ~/Pictures/Lza1qCX.png"

    -- Applets
    spawn "nm-applet" -- ネット接続のアプレットを起動
    spawn "fcitx"
    spawn "blueman-applet"
    -- gnome-sound-appletのアイコンが黒一色でない場合は--transparent trueにすると統一感があっていいです。 -- GNOMEのトレイを起動 -- XXX(sleep 2): #6: Trayer broken with nautilus
    spawn "sleep 5; killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 5 --widthtype percent --transparent true --tint 0x4E4B42 --height 30 --alpha 0 --monitor 0"

    -- Workaround for Java apps
    spawn "wmname LG3D"

--    spawn "compton -b --config ~/.comptonrc"

    spawn "streamdeck -n"

    io (threadDelay (2 * 1000 * 1000))
    numDisplayStr <- runProcessWithInput "sh" ["-c", "xrandr --query | grep -c '\\bconnected\\b'"] ""
    let numDisplay = read numDisplayStr :: Int
    spawn $ "echo '" ++ (show numDisplay) ++ "' > /tmp/test"
    spawn $ "xrandr --query | grep -c '\\bconnected\\b' >> /tmp/test"
    xmprocs <- mapM (\displayId -> spawnPipe $ "export FONTCONFIG_FILE=" ++ homeDirectory ++ "/.xmobar/font.conf && /usr/bin/xmobar -D 120 " ++ (if displayId == 0 then "" else "-p 'TopSize L 100 30' -x " ++ (show displayId)) ++ " ~/.xmobarrc") [0..numDisplay-1]

    spawn "gnome-screensaver"
    spawn "pulseeffects --gapplication-service"
    spawn "killall dunst"

    spawn "xrandr --output eDP-1 --brightness 1 --gamma 1.05:1.05:1.095"

    spawn "CM_MAX_CLIPS=10000 CM_DIR=$HOME CM_SELECTIONS=clipboard CM_IGNORE_WINDOW=xmonad.terminal.action.one.password clipmenud"

    spawn "~/.xmonad/system_scripts/bright/sync.sh"

--    spawn $ "echo '" ++ (show $ mkToggleInitial (single TitleTransformer) TitleTransformer $ myLayout) ++ "' >> /tmp/xmonad.debug.layout"
    xmonad $ baseConfig
        { startupHook = myStartupHook
        , manageHook = myManageHookAll
        , layoutHook =  myLayoutHookAll
        , logHook = measure "logHook" $ myLogHook $ xmobarLogHook xmprocs
        , handleEventHook = \e -> measure "handleEventHook" $ myHandleEventHook e
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = 4
        , normalBorderColor  = blue
        , focusedBorderColor = red
        , focusFollowsMouse = False -- マウスの移動でフォーカスが映らないように
        , clickJustFocuses = False
        , XMonad.Core.workspaces = myWorkspaces
        --, clientMask = keyPressMask
        , rootMask = (rootMask baseConfig) .|. buttonReleaseMask
        } `additionalKeys` (L.concat $ [
          systemKeys
        , dunstKeys
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
        , workspaceHistoryKeys
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
        , ("<XF86LaunchA>", withoutLogHook $ showOrHideScratchpads myScratchpads False)
        , ("<XF86LaunchB>", withoutLogHook $ showOrHideScratchpads myScratchpads True)
        ] `removeKeys` [
          (mod4Mask .|. shiftMask, xK_q)
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
        , ((0, 8), \w -> do
             windowViewState <- XS.get
             case windowViewState of
               Normal -> prevWS'
               WindowView -> windows floatAvoidFocusUp)
        , ((0, 9), \w -> do
             windowViewState <- XS.get
             case windowViewState of
               Normal -> nextWS'
               WindowView -> windows floatAvoidFocusDown)
        , ((0, 10), \w -> whenDoubleClick (0.300 :: NominalDiffTime) 10 10 nextVirtualScreen)
        , ((0, 14), \w -> do
             focus w
             quitWindowView $ sendMessage ToggleLayout)
        , ((0, 11), \w -> windows floatAvoidFocusDown)
        , ((0, 15), \w -> return ())
        , ((0, 12), \w -> spawn $ "xdotool key XF86Copy")
        , ((0, 13), \w -> spawn $ "xdotool key XF86Paste")
        ]
        `advancedMouseBindings` [
          ([10], (0, 8), \w -> prevVirtualScreen)
        , ([10], (0, 9), \w -> nextVirtualScreen)
        , ([10], (0, 1), \w -> focus w >> kill)
        , ([10], (0, 4), \w -> windows floatAvoidFocusUp)
        , ([10], (0, 5), \w -> windows floatAvoidFocusDown)
        ] `ungrabButtons'` [1, 2, 3, 4, 5]

-- Libraries

------------------------------------------------------------------------------------------
-- XMonad utils
------------------------------------------------------------------------------------------

-- | Modify the @WindowSet@ in state with no special handling.
--modifyWindowSet :: (WindowSet -> WindowSet) -> X ()
--modifyWindowSet f = modify $ \xst -> xst { windowset = f (windowset xst) }

------------------------------------------------------------------------------------------
-- XMonad utils
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- WorkspaceFamily
------------------------------------------------------------------------------------------
originalWorkspaces = map show ([1 .. 9 :: Int] ++ [0])
workspaceFamilies = map show ([1 .. 9 :: Int] ++ [0])
myWorkspaces = expandWorkspacesToFamily workspaceFamilies originalWorkspaces

multiScreenXMobarPP windowSet screenId header xmproc = xmobarPP
                        { ppOutput = \t -> hPutStrLn xmproc $ header ++ " | " ++ (fallbackIfNoScreen (\ws -> \sid -> \fid -> fid) windowSet screenId) ++ " | " ++ t ++ "  |  "
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

showOnlyWorkspaceFor f windowSet screenId fid = \w ->
                                  case readWorkspaceInFamily w of
                                    Just wif -> if familyId wif == fid then
                                                    f windowSet screenId $ workspaceId wif
                                                else ""
                                    Nothing -> f windowSet screenId w

screenIds windowSet xmobarScreenId =
    L.foldr (\a -> \b -> a ++ b) "" $ L.map
         (\sid ->
              xmobarColor'
              (wrap (if xmobarScreenId == sid then "[" else " ") (if xmobarScreenId == sid then "]" else " ") $
                    show $ sid + 1)
              black white $
                        (fromIntegral sid) == (W.screen $ W.current windowSet)) [0 .. L.length $ W.visible windowSet]
--                                                                   xmobarColor' (wrap " " " " $ xmobarColor' (show sid) black white $ xmobarScreenId == sid) black white $ (fromIntegral sid) == (W.screen $ W.current windowSet)) [0 .. L.length $ W.visible windowSet]

fallbackIfNoScreen f windowSet screenId =
  let (sid, tag) = case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
                     Just sc -> (screenId, W.tag $ W.workspace $ sc)
                     Nothing -> (0, W.tag $ W.workspace $ W.current windowSet)
  in f windowSet sid $ (fromMaybe tag $ toFamilyIdMaybe tag)

viewToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.view . W.tag . W.workspace

greedyViewToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.greedyView . W.tag . W.workspace

shiftToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.shift . W.tag . W.workspace

------------------------------------------------------------------------------------------
-- Workspaces
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
