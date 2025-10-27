module XMonad.Conf.MyKeys where

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Graphics.X11.Xlib

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

meta = mod4Mask
altMask = mod1Mask
alt = altMask
shft = shiftMask
ctrl = controlMask

bindKeys :: [(KeyMask, KeySym)] -> X () -> [((KeyMask, KeySym), X ())]
bindKeys keys x = L.map (\k -> (k, x)) keys

bindKey :: KeyMask -> KeySym -> X () -> [((KeyMask, KeySym), X ())]
bindKey mask key x = [((mask, key), x)]

configureKeys config = config `additionalKeys` myKeys `additionalKeysP` myKeysP `removeKeys` myRemovedKeys

myKeys = L.concat $ [
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
        ]
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
myRemovedKeys = [(mod4Mask .|. shiftMask, xK_q)]

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
viewToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.view . W.tag . W.workspace

greedyViewToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.greedyView . W.tag . W.workspace

shiftToScreen screenId = do
    withWindowSet $ \s -> caseMaybeJust (L.find (\sc -> (W.screen sc) == S (screenId - 1)) $ W.screens s) $ windows . W.shift . W.tag . W.workspace


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
  , ((mod4Mask .|. shiftMask, xK_g), withWindowSet $ \ws ->
         sendMessage $ GridLayout $ max 1 $ L.length $ W.integrate' $ W.stack $ W.workspace $ W.current ws
    )

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
