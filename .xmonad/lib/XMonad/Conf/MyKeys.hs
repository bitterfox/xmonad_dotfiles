module XMonad.Conf.MyKeys where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Bits

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Misc (keysymToString)

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.EZConfig
import XMonad.Actions.Submap
import XMonad.Layout.ToggleLayouts
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Actions.Search (selectSearchBrowser, google)

import XMonad.Layout.CompositeTall
import XMonad.Layout.WindowViewableLayout
import XMonad.Layout.MyMultiToggle
import XMonad.Util.SwitchableLogHook
import XMonad.Util.PhysicalScreen
import XMonad.Util.VirtualScreen
import XMonad.Actions.EvacuationLikeMac
import XMonad.Actions.IntelliJTerminal
import XMonad.Util.MyUtils
import XMonad.Util.DocksSupport
import XMonad.Util.VirtualMouse
import XMonad.Actions.FloatAwareFocus
import XMonad.Util.WorkspaceFamily
import XMonad.Util.ManageHookUtils
import XMonad.Actions.DrawShape
import XMonad.Util.WorkspaceHistory
import XMonad.Util.MyNamedScratchpad

import XMonad.Conf.MyConf
import XMonad.Conf.MyHook
import XMonad.Conf.MyTerminalAction
import XMonad.Conf.MyTerminalAction.Fallback

altMask = mod1Mask

shft = shiftMask
ctrl = controlMask
alt = altMask
super = mod4Mask
hyper = mod5Mask

bindKeys :: [(KeyMask, KeySym)] -> X () -> [((KeyMask, KeySym), X ())]
bindKeys keys x = L.map (\k -> (k, x)) keys
bindKeysWithDescription :: String -> [(KeyMask, KeySym)] -> X () -> [(String, (KeyMask, KeySym), X ())]
bindKeysWithDescription desc keys x = L.map (\k -> (desc, k, x)) keys

bindKey :: KeyMask -> KeySym -> X () -> [((KeyMask, KeySym), X ())]
bindKey mask key x = [((mask, key), x)]
bindKeyWithDescription :: String -> KeyMask -> KeySym -> X () -> [(String, (KeyMask, KeySym), X ())]
bindKeyWithDescription desc mask key x = [(desc, (mask, key), x)]

configureKeys config = config `removeKeys` myRemovedKeys `additionalKeys` myKeys `additionalKeys` (hyperCompatible myKeys $ super .|. alt) `additionalKeysP` myKeysP

myKeysWithDescription = [
    systemKeys
  , dunstKeys
  , windowKeys
  , floatWindowKeys
  , layoutKeys
  , workspaceHistoryKeys
  , screenKeys
  , virtualScreenKeys
  , functionKeys
  , terminalActionKeys
  , scratchpadKeys
  , gridSelectedKeys
  , drawShapeKeys
  , utilKeys
  , vmwareSupportKeys
  ]

myKeys = myKeysWithDescription >>= keysWithoutDescription
myKeysP = [-- 輝度・ボリューム周り
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
        ]
myRemovedKeys = [(super .|. shft, xK_q)]

systemKeys = ("System keys", [
    ("System actions (Terminal action)", (super, xK_q), myRunSelectedXTerminalAction systemActions)
  , ("Reload", (super .|. shft, xK_q), myrestart)
  , ("System actions (Grid select)",     (super .|. alt, xK_q), runActionSelected hidpiGSConfig systemActions)
  , ("Refresh",                          (super, xK_r), myrefresh)
  -- Screenshot
  , ("Screenshot (whole screens)",       (0, xK_Print), spawn "sh ~/.xmonad/screenshot.sh")
  , ("Screenshot (whole screens)",       (super, xK_s), spawn "sh ~/.xmonad/screenshot.sh")
  , ("Screenshot (selected area)",       (super .|. shft, xK_s), spawn "sh ~/.xmonad/screenshot.sh -a")

  -- Performance
  -- , ((super, xK_a), do
  --      durations <- getDurations
  --      spawn $ "echo '" ++ (show durations) ++ "' >> /tmp/xmonad.perf")
  ])

myrefresh = withWindowSet $ \ws -> do
  let sid = W.screen $ W.current ws
  viewScreen 0
  refresh
  rePhysicalScreen priorityDisplayEDIDs
  docksStartupHook
  resetVirtualScreens
  initializeScreenMouses
  viewScreen sid

dunstKeys = ("Notification control (Dunst)", [
    ("Close latest notification",  (super                  , xK_slash), spawn "dunstctl close")
  , ("Close all notifications",    (super .|. shft .|. ctrl, xK_slash), spawn "dunstctl close-all")
  , ("Pop from history",           (super .|. shft         , xK_slash), spawn "dunstctl history-pop")
  , ("Toggle pause notifications", (super          .|. ctrl, xK_slash), spawn "dunstctl set-paused toggle")
  , ("Context menu",               (hyper                  , xK_slash), spawn "dunstctl context")
  ])

windowKeys = ("Manage windows", L.concat $ [
  -- Emacs binding
  -- Window $ Workspace
  -- Focus window
    bindKeysWithDescription "Focus prev tiled window"
        [(super, xK_p)
        ,(super, xK_Up)] $ windows floatAvoidFocusUp
  , bindKeysWithDescription "Focus next tiled window"
        [(super, xK_n)
        ,(super, xK_Down)] $ windows floatAvoidFocusDown
  -- Focus workspace
  , bindKeysWithDescription "Focus prev workspace"
        [(super, xK_b)
        ,(super, xK_Left)] $ do
      windowViewState <- XS.get
      case windowViewState of
        Normal -> prevWS'
        WindowView -> windows floatAvoidFocusUp
  , bindKeysWithDescription "Focus next workspace"
        [(super, xK_f)
        ,(super, xK_Right)] $ do
      windowViewState <- XS.get
      case windowViewState of
        Normal -> nextWS'
        WindowView -> windows floatAvoidFocusDown
  -- Shift window
  , bindKeysWithDescription "Swap window with prev window"
        [(super .|. shft, xK_p)
        ,(super .|. shft, xK_Up)] $ windows floatAvoidSwapUp
  , bindKeysWithDescription "Swap window with next window"
        [(super .|. shft, xK_n)
        ,(super .|. shft, xK_Down)] $ windows W.swapDown
  , bindKeysWithDescription "Move window to prev workspace"
        [(super .|. shft, xK_b)
        ,(super .|. shft, xK_Left)] $ shiftToPrevWS' >> prevWS'
  , bindKeysWithDescription "Move window to next workspace"
        [(super .|. shft, xK_f)
        ,(super .|. shft, xK_Right)] $ shiftToNextWS' >> nextWS'
  -- Master
  , bindKeyWithDescription "Focus master window" super xK_m $ windows W.focusMaster
  , bindKeyWithDescription "Move window to master" (super .|. shft) xK_m $ windows W.shiftMaster
  , bindKeyWithDescription "Swap window with master window" (super .|. shft .|. ctrl) xK_m $ windows W.swapMaster

  , [(if m == shft then "Move window to workspace " ++ i else "Greedy view workspace" ++ i,
             (super .|. m, k), f i)
             | (i, k) <- zip originalWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
             , (f, m) <- [(greedyViewToWorkspace, 0), (shiftToWorkspace, shft)]
    ]
  , [((if m == ctrl .|. shft then "Move window to" else "Greedy view") ++ " workspace (2nd stroke) in the family " ++ i,
             (super .|. m, k), f i)
             | (i, k) <- zip workspaceFamilies $ [xK_1 .. xK_9] ++ [xK_0]
             , (f, m) <- [
               (\family -> submap . M.fromList $
                             [((0, subkey), greedyViewToFamilyWorkspace family workspace)
                               | (workspace, subkey)  <- zip originalWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
                             ], ctrl)
             , (\family -> submap . M.fromList $
                             [((0, subkey), shiftToFamilyWorkspace family workspace)
                               | (workspace, subkey) <- zip originalWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
                             ], (ctrl .|. shft))
             ]
    ]
  ])

floatWindowKeys = ("Manage float windows", [
    ("Focus prev float window",            (super .|. shft         , xK_t), windows floatFocusUp)
  , ("Focus next float window",            (super                  , xK_t), windows floatFocusDown)
  , ("Push float window back into tiling", (super          .|. ctrl, xK_t), withFocused $ windows . W.sink)
  ])

screenKeys = ("Manage screens", L.concat $ [
    bindKeysWithDescription "Focus prev physical screen" [(super .|. alt, xK_b)
             ,(super .|. shft, xK_space)] $ prevVirtualScreen
  , bindKeysWithDescription "Focus next physical screen" [(super .|. alt, xK_f)
             ,(super, xK_space)] $ nextVirtualScreen
  , bindKeyWithDescription "Move window to prev physical screen" (super .|. shft .|. alt) xK_b $ shiftPrevRootScreen >> prevVirtualScreen
  , bindKeyWithDescription "Move window to next physical screen" (super .|. shft .|. alt) xK_f $ shiftNextRootScreen >> nextVirtualScreen
  -- M+Alt 1~0: View screen
  -- M+Alt+Ctrl 1~0: Greedy view to screen
  -- M+Alt+Shift 1~0: Shift to screen
  , [
          ((if m == alt then "View" else if m == alt .|. ctrl then "Greedy view" else "Move window to") ++ " screen " ++ (show i),
                  (super .|. m, k), f i)
                  | (i, k) <- zip [1..9] [xK_1 .. xK_9]
                  , (f, m) <-[(viewToScreen, alt), (greedyViewToScreen, alt .|. ctrl), (shiftToScreen, alt .|. shft)]
    ]
  ])
viewToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.view . W.tag . W.workspace

greedyViewToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.greedyView . W.tag . W.workspace

shiftToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.shift . W.tag . W.workspace

virtualScreenKeys = ("Manage virtual screen", [
    ("Create new virtual screen in this physical screen",         (super, xK_x), createVirtualScreen' (myLayoutForVirtualScreen ||| (Mirror myLayoutForVirtualScreen)) $ selectUnusedFamilyWorkspace)
  , ("Create remove virtual screen",                              (super .|. ctrl, xK_x), removeVirtualScreen)
  , ("Reset virtual screen in this physical screen",              (super .|. shft, xK_x), resetVirtualScreen)

  , ("Focus prev virtual screen",                                 (hyper, xK_p), prevChildScreen)
  , ("Focus next virtual screen",                                 (hyper, xK_n), nextChildScreen)

  -- Layout
  , ("Layout: Next layout of virtual screen",                     (super .|. alt, xK_d), sendScreenMessage NextLayout)
  , ("Layout: Increase number of virtual screens in current row", (super .|. alt .|. shft, xK_comma    ), sendScreenMessageToCompositeTall (IncMasterN 1))
  , ("Layout: Decrease number of virtual screens in current row", (super .|. alt .|. shft, xK_period), sendScreenMessageToCompositeTall (IncMasterN (-1)))
  , ("Layout: New virtual screens row at left",                   (super .|. alt .|. ctrl, xK_comma     ), sendScreenMessageToCompositeTall NewCellAtLeft)
  , ("Layout: New virtual screens row at right",                  (super .|. alt .|. ctrl, xK_period ), sendScreenMessageToCompositeTall NewCellAtRight)
  , ("Layout: Shrink current virtual screen",                     (super .|. alt, xK_j), sendScreenMessageToCompositeTall Shrink)
  , ("Layout: Expand current virtual screen",                     (super .|. alt, xK_l), sendScreenMessageToCompositeTall Expand)
  , ("Layout: Expand current virtual screen (another side)",      (super .|. alt .|. shft, xK_j), sendScreenMessageToCompositeTall $ ResizeAnotherSide Expand)
  , ("Layout: Shrink current virtual screen (another side)",      (super .|. alt .|. shft, xK_l), sendScreenMessageToCompositeTall $ ResizeAnotherSide Shrink)
  , ("Layout: Shrink current virtual screen row",                 (super .|. alt, xK_i), sendScreenMessageToCompositeTall $ DelegateMessage $ SomeMessage Shrink)
  , ("Layout: Expand current virtual screen row",                 (super .|. alt, xK_k), sendScreenMessageToCompositeTall $ DelegateMessage $ SomeMessage Expand)
  , ("Layout: Shrink current virtual screen row (another side)",  (super .|. alt .|. shft, xK_i), sendScreenMessageToCompositeTall $ DelegateMessage $ SomeMessage $ ResizeAnotherSide Expand)
  , ("Layout: Expand current virtual screen row (another side)",  (super .|. alt .|. shft, xK_k), sendScreenMessageToCompositeTall $ DelegateMessage $ SomeMessage $ ResizeAnotherSide Shrink)
  ])

sendScreenMessageToCompositeTall msg = do
  vs <- currentVirtualScreen
  caseMaybeJust vs $ \it -> do
    let pos = L.length $ W.up $ screenStack it
    sendScreenMessage $ CompositeTallMessage {message = SomeMessage $ msg, messageAtWindow = pos}

layoutKeys = ("Manage windows layout", [
  -- Layout
    ("Layout: Next layout of windows", (super, xK_d), sendMessage NextLayout)
  , ("Layout: Full screen", (super, xK_v), quitWindowView $ sendMessage ToggleLayout)

  , ("Layout: Increase number of windows in current row", (super .|. shft, xK_comma    ), sendMessage (IncMasterN 1))
  , ("Layout: Decrease number of windows in current row", (super .|. shft, xK_period), sendMessage (IncMasterN (-1)))
  , ("Layout: New windows row at left",  (super .|. ctrl, xK_comma     ), sendMessage NewCellAtLeft)
  , ("Layout: New windows row at right", (super .|. ctrl, xK_period ), sendMessage NewCellAtRight)

  , ("Layout: Increase number of windows in current row", (super         , xK_minus), sendMessage (IncMasterN 1))
  , ("Layout: Decrease number of windows in current row", (super .|. shft, xK_minus), sendMessage (IncMasterN (-1)))
  , ("Layout: New windows row at left",  (super .|. shft, xK_asciicircum), sendMessage NewCellAtLeft)
  , ("Layout: New windows row at right", (super         , xK_asciicircum), sendMessage NewCellAtRight)

  , ("Layout: Grid layout of windows", (super .|. shft, xK_g), withWindowSet $ \ws ->
         sendMessage $ GridLayout $ max 1 $ L.length $ W.integrate' $ W.stack $ W.workspace $ W.current ws
    )
  , ("Layout: Full screen at current row", (super .|. ctrl, xK_v), sendMessage $ DelegateMessage $ SomeMessage $ ToggleLayout)

  -- Struts
  , ("Layout: Toggle struts bar", (super, xK_h), docksOnBottom >> (sendMessage ToggleStruts))
  , ("Layout: Toggle title bar", (super .|. shft, xK_h), sendMessage $ XMonad.Layout.MyMultiToggle.Toggle TitleTransformer)

  -- Window view
  , ("Layout: Window view", (super .|. shft, xK_v), startWindowView)

  -- Sizing
  , ("Layout: Shrink tiled window row or move float window to left",
      (super, xK_j),
        whenX (xnot $ locateFloat $ onLeft)
          $ sendMessage Shrink)
  , ("Layout: Expand tiled window row or move float window to right",
      (super, xK_l),
        whenX (xnot $ locateFloat $ onRight)
          $ sendMessage Expand)
  , ("Layout: Shrink tiled window or move float window to top",
      (super, xK_i),
        whenX (xnot $ locateFloat $ onTop)
          $ sendMessage $ DelegateMessage $ SomeMessage Shrink)
  , ("Layout: Expand tiled window or move float window to bottom",
      (super, xK_k),
        whenX (xnot $ locateFloat $ onBottom)
          $ sendMessage $ DelegateMessage $ SomeMessage Expand)

  , ("Layout: Expand tiled window row (another side)", (super .|. shft, xK_i), sendMessage $ DelegateMessage $ SomeMessage $ ResizeAnotherSide Expand)
  , ("Layout: Shrink tiled window row (another side)", (super .|. shft, xK_k), sendMessage $ DelegateMessage $ SomeMessage $ ResizeAnotherSide Shrink)
  , ("Layout: Expand tiled window (another side)", (super .|. shft, xK_j), sendMessage $ ResizeAnotherSide Expand)
  , ("Layout: Shrink tiled window (another side)", (super .|. shft, xK_l), sendMessage $ ResizeAnotherSide Shrink)

  , ("Layout: Reset size", (super .|. shft, xK_r), sendMessage ResetSize)
  ])

scratchpadKeys = ("NamedScratchpads", [
  -- Scratchpad
    ("rhythmbox", (super, xK_F4), myNamedScratchpadAction "rhythmbox")
  , ("main terminal", (super, xK_Return), myNamedScratchpadAction "mainterm")
  , ("", (super, xK_F9), myNamedScratchpadAction "艦これ")
  , ("", (super, xK_F10), myNamedScratchpadAction "bunnaru")
  , ("top terminal", (super, xK_bracketleft), myNamedScratchpadAction "term1")
  , ("bottom terminal", (super, xK_bracketright), myNamedScratchpadAction "term2")
  , ("left terminal", (super .|. shft, xK_bracketleft), myNamedScratchpadAction "termL")
  , ("right terminal", (super .|. shft, xK_bracketright), myNamedScratchpadAction "termR")
  , ("top jshell", (super .|. ctrl, xK_bracketleft), myNamedScratchpadAction "jshell1")
  , ("bottom jshell", (super .|. ctrl, xK_bracketright), myNamedScratchpadAction "jshell2")

--  , ((super, xK_a), myNamedScratchpadAction "ai-chatgpt")
--  , ((super .|. shft, xK_a), myNamedScratchpadAction "ai-gemini")
  , ("ChatGPT", (hyper, xK_a), myNamedScratchpadAction "ai-chatgpt")
  , ("Gemini", (hyper .|. shft, xK_a), myNamedScratchpadAction "ai-gemini")

  , ("Toggle scratch pads (reverse)", (super .|. ctrl, xK_F7), toggleScrachpadAction $ L.reverse myScratchpads)
  , ("Hide scratchpads", (super .|. ctrl, xK_F8), withoutLogHook $ showOrHideScratchpads myScratchpads True)
  , ("Show scratchpads", (super .|. ctrl .|. shft, xK_F8), withoutLogHook $ showOrHideScratchpads myScratchpads False)
  , ("Toggle scratch pads", (super .|. ctrl, xK_F9), toggleScrachpadAction myScratchpads)
  ])

terminalActionKeys = ("Terminal actions", [
  -- TerminalAction
    ("Smart greedy view selected window", (super, xK_w),           smartGreedyViewSelectedWindowTerminalAction windowPredicates)
  , ("Greedy view selected window", (super .|. ctrl, xK_w),  greedyViewSelectedWindowTerminalAction      windowPredicates)
  , ("Move selected window to current workspace", (super .|. shft, xK_w),  shiftSelectedWindowTerminalAction           windowPredicates)
  , ("Execute selected application", (super, xK_e),           spawnAppSelectedTerminalAction' applications)
  , ("Open dashboard", (super, xK_at),          runOpenDashboardTerminalAction)
  , ("Open another dashboard", (super .|. shft, xK_at), runOpenITerminalAction)
  , ("Open IntelliJ project", (super, xK_colon),       openIntelliJTerminalAction)
  , ("Open browser history", (super, xK_semicolon),   runOpenBrowserHistoryTerminalAction)
  , ("Open browser tab", (super .|. shft, xK_semicolon),   runOpenBrowserTabTerminalAction)
  , ("Open clipboard history", (super, xK_c),           runCopyFromClipboardHistoryTerminalAction)
  , ("Open 1password", (super .|. ctrl, xK_c),  runOnePasswordTerminalAction)
  ])
  where windowPredicates = [
          ("All workspaces", anyWorkspacePredicate),
          ("Visible workspaces", visibleWorkspacesPredicate),
          ("Workspace for current family", anyWorkspaceInCurrentWorkspaceFamilyPredicate)]

functionKeys = ("Functions", [
  -- Functions
    ("Audio mute", (super              , xK_F1), spawn "sh ~/.xmonad/audio_mute.sh")
  , ("Audio down", (super              , xK_F2), spawn "sh ~/.xmonad/audio_down.sh")
  , ("Audio up", (super              , xK_F3), spawn "sh ~/.xmonad/audio_up.sh")
  , ("Audio prev", (super .|. shft, xK_F2), spawn "sh ~/.xmonad/audio_prev.sh")
  , ("Audio next", (super .|. shft, xK_F3), spawn "sh ~/.xmonad/audio_next.sh")
  -- F4: rhythmbox
  , ("Brightness down", (super              , xK_F5), spawn "sh ~/.xmonad/system_scripts/bright/down.sh")
  , ("Brightness up", (super              , xK_F6), spawn "sh ~/.xmonad/system_scripts/bright/up.sh")
  , ("CPU freq min up", (super              , xK_F7), spawn "intel-pstate-utils-update-freq min up; killall -SIGUSR1 xmobar_metrics_daemon")
  , ("CPU freq min down", (super .|. shft, xK_F7), spawn "intel-pstate-utils-update-freq min down; killall -SIGUSR1 xmobar_metrics_daemon")
  , ("CPU freq max up", (super              , xK_F8), spawn "intel-pstate-utils-update-freq max up; killall -SIGUSR1 xmobar_metrics_daemon")
  , ("CPU freq max down", (super .|. shft, xK_F8), spawn "intel-pstate-utils-update-freq max down; killall -SIGUSR1 xmobar_metrics_daemon")
  -- F9: 艦これ
  -- F10: 文ある
  ])

gridSelectedKeys = ("Grid selected", [
    ("Go to selected window in current workspace family", (super                   .|. alt, xK_w), goToSelected'  anyWorkspaceInCurrentWorkspaceFamilyPredicate hidpiGSConfig)
  , ("Go to selected window in any workspace",            (super          .|. ctrl .|. alt, xK_w), goToSelected'  anyWorkspacePredicate                         hidpiGSConfig)
  , ("Shift selected window in current workspace family", (super .|. shft          .|. alt, xK_w), shiftSelected' anyWorkspaceInCurrentWorkspaceFamilyPredicate hidpiGSConfig)
  , ("Shift selected window in any workspace",            (super .|. shft .|. ctrl .|. alt, xK_w), shiftSelected' anyWorkspacePredicate                         hidpiGSConfig)
  , ("Spawn selected application",                        (super                   .|. alt, xK_e), spawnAppSelected hidpiGSConfig applications)
  ])

drawShapeKeys = ("Draw shape", [
    ("Draw outlined rectangle",   (super                          , xK_z), drawShapeOnMouse $ DrawShape OutlinedRectangle 5 red_rgb)
  , ("Draw filled rectangle",     (super .|. shft                 , xK_z), drawShapeOnMouse $ DrawShape FilledRectangle 5 red_rgb)
  , ("Draw line",                 (super          .|. ctrl        , xK_z), drawShapeOnMouse $ DrawShape LongestStraightLine 5 red_rgb)
  , ("Remove latest drawn shape", (super                   .|. alt, xK_z), removeLatestDrawnShape)
  ])

utilKeys = ("Util", [
    ("gmrun",                (super .|. shft, xK_e), spawn "gmrun")
  , ("Google selected text", (super, xK_g), selectSearchBrowser "/usr/bin/vivaldi" google)
  , ("IntelliJ terminal",    (super, xK_backslash), launchIntelliJTerminal intelliJTerminalEnv)
  ])

workspaceHistoryKeys = ("Workspace history", [
    ("Undo workspace history", (super, xK_comma ), undoWorkspaceHistory)
  , ("Redo workspace history", (super, xK_period), redoWorkspaceHistory)
  ])

vmwareSupportKeys = ("VMWare", [
   ("Mission control", (0, xK_F19), spawn "echo 'F19' >> /mnt/hgfs/data/mission-control")
 ])

hyperCompatible keys compatibleModifier =
    [ (((mods .&. complement hyper) .|. compatibleModifier, key), action)
          | ((mods, key), action) <- keys
          , mods .&. hyper /= 0
    ]

keysWithoutDescription (description, keys) =
    [ ((mods, key), action) | (desc, (mods, key), action) <- keys]

describeMyKeys :: X ()
describeMyKeys = liftIO $ describeKeys myKeysWithDescription

describeKeys keysWithDescriptions = do
  home <- getHomeDirectory
  let path = home </> ".xmonad" </> "keys.md"
  writeFile path $ unlines $ describeKeysMarkdown keysWithDescriptions

describeKeysMarkdown ((description, keys):rest) = do
    let title = "# " ++ description

    let header = "| Key | Action |"
    let headerBorder = "| -- | -- |"
    let keysString = [ "| " ++ (keyDescription desc mods key) ++ " | " ++ desc ++ " |"
         | (desc, (mods, key), action) <- keys
         ]

    [title, "", header, headerBorder] ++ keysString ++ [""] ++ (describeKeysMarkdown rest)

describeKeysMarkdown [] = []

defaultKeyDescription desc mods key = do
  wrapMods $ keysymToString key
  where superStr = if mods .&. super /= 0 then "⌘" else ""
        hyperStr = if mods .&. hyper /= 0 then "✧" else ""
        shiftStr = if mods .&. shft /= 0 then "⇧" else ""
        controlStr = if mods .&. ctrl /= 0 then "⌃" else ""
        altStr = if mods .&. alt /= 0 then "⌥" else ""
        modsStr = superStr ++ hyperStr ++ shiftStr ++ controlStr ++ altStr
        wrapMods key = if null modsStr then key else modsStr ++ "+" ++ key

keyDescription desc mods key
  | mods == (super .|. ctrl) && (key `L.elem` [xK_1 .. xK_9] ++ [xK_0])
  = defaultKeyDescription desc mods key ++ ", 0..9"
  | mods == (super .|. shft .|. ctrl) && (key `L.elem` [xK_1 .. xK_9] ++ [xK_0])
  = defaultKeyDescription desc mods key ++ ", 0..9"
  | otherwise
  = defaultKeyDescription desc mods key
