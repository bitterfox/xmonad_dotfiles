{-# LANGUAGE DeriveDataTypeable #-}
import XMonad
import XMonad.Core
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
import XMonad.Actions.GridSelect
import XMonad.Actions.Search (selectSearchBrowser, google)
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo
--import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutScreens
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Roledex
import qualified XMonad.StackSet as W
import XMonad.Layout.ToggleLayouts
import XMonad.ManageHook
import XMonad.Util.Run(spawnPipe, runProcessWithInput, runProcessWithInputAndWait, seconds)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import qualified XMonad.Util.ExtensibleState as XS
import System.Exit
import System.IO
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Control.Exception.Extensible as E
import Control.Monad (foldM, filterM, mapM, forever)
import Control.Concurrent
import qualified Text.Show as TS
import Text.Parsec
import Text.Parsec.String (Parser)

black = "#4E4B42"
brightBlack = "#635F54"
gray = "#B4AF9A"
darkWhite = "#CDC8B0"
white = "#DAD4BB"
red = "#CC654C"
blue = "#3BA99F"

applications = [
 "vivaldi",
 "nautilus",
 "emacs",
 "wine '/home/bitterfox/.wine/drive_c/users/bitterfox/Local Settings/Application Data/LINE/bin/LineLauncher.exe'",
 "XDG_CURRENT_DESKTOP=GNOME gnome-control-center",
 "libreoffice",
 "~/bin/jetbrains-toolbox-1.14.5179/jetbrains-toolbox",
 "~/bin/idea",
 "/usr/local/pulse/pulseUi",
 "slack"]

myManageHookAll = manageHook gnomeConfig -- defaultConfig
                       <+> manageDocks
                       <+> myScratchpadsManageHook
                       <+> (className =? "Dunst" --> doFloat)
                       <+> ((fmap (L.isSuffixOf ".onBottom") appName) --> onBottom)
myLayout = (ResizableTall 1 (3/100) (1/2) [])
myLayoutHookAll = avoidStruts $
                       toggleLayouts (renamed [Replace "■"] $ noBorders Full) $
-- $                       ((renamed [Replace "┣"] $ noFrillsDeco shrinkText mySDConfig $ myLayout) ||| (renamed [Replace "┳"] $ noFrillsDeco shrinkText mySDConfig $ Mirror myLayout) ||| (renamed [Replace "田"] $ noFrillsDeco shrinkText mySDConfig $ multiCol [1] 4 0.01 0.5)) -- tall, Mirror tallからFullにトグルできるようにする。(M-<Sapce>での変更はtall, Mirror tall) --  ||| Roledex
                       (   (renamed [Replace "┣"] $ noFrillsDeco shrinkText mySDConfig $ myLayout)
                       ||| (renamed [Replace "┳"] $ noFrillsDeco shrinkText mySDConfig $ Mirror myLayout)
                       ||| (renamed [Replace "田"] $ multiCol [1] 4 0.01 0.5)) -- tall, Mirror tallからFullにトグルできるようにする。(M-<Sapce>での変更はtall, Mirror tall) --  ||| Roledex

tall = Tall 1 (3/100) (1/2)

watch :: String -> String -> IO ()
watch cmd interval = spawn $ "while :; do " ++ cmd ++ "; sleep " ++ interval ++ "; done"

main = do
    spawn $ "mkdir -p " ++ mouseLogDir

--    watch "xmodmap ~/.xmodmap" "0.3"
    spawn "xhost +SI:localuser:root; sleep 1; sudo xkeysnail -q ~/config.py & sleep 1; xset r rate 250 50"
    spawn "sudo libinput-gestures"

    spawn "nautilus-desktop --force" -- デスクトップを読み込む

--    spawn "killall trayer ; sleep 2 ; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 10 --widthtype percent --transparent false --tint 0x000000 --height 22" -- gnome-sound-appletのアイコンが黒一色でない場合は--transparent trueにすると統一感があっていいです。 -- GNOMEのトレイを起動 -- XXX(sleep 2): #6: Trayer broken with nautilus

--    spawn "gnome-power-manager"
    spawn "nm-applet" -- ネット接続のアプレットを起動
--    spawn "gnome-sound-applet" -- gnome-volume-control-applet? -- ボリューム変更のアプレットを起動
--    spawn "bluetooth-applet"
--    spawn "sparkleshare restart"
--  spawn "/opt/toggldesktop/TogglDesktop.sh"
    spawn "fcitx"

    -- gnome-sound-appletのアイコンが黒一色でない場合は--transparent trueにすると統一感があっていいです。 -- GNOMEのトレイを起動 -- XXX(sleep 2): #6: Trayer broken with nautilus
--    spawn "sleep 5; killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 5 --widthtype percent --transparent true --tint 0x4E4B42 --height 28 --alpha 0 --monitor 0; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 5 --widthtype percent --transparent true --tint 0x4E4B42 --height 28 --alpha 0 --monitor 1 ;dropbox start"
--    spawn "sleep 5; killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 5 --widthtype percent --transparent true --tint 0x4E4B42 --height 28 --alpha 0 --monitor 0"
    spawn "sleep 5; killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 5 --widthtype percent --transparent true --tint 0x4E4B42 --height 28 --alpha 0 --monitor 0"

    spawn "wmname LG3D"

    spawn "/home/bitterfox/xmonad_dotfiles/.xmonad/always_dunst_on_top.sh"

--    spawn "compton -b --config ~/.comptonrc"

    numDisplayStr <- runProcessWithInput "sh" ["-c", "xrandr --query | grep -c '\\bconnected\\b'"] ""
    let numDisplay = read numDisplayStr :: Int
    spawn $ "echo '" ++ (show numDisplay) ++ "' > /tmp/test"
    spawn $ "xrandr --query | grep -c '\\bconnected\\b' >> /tmp/test"
    xmprocs <- mapM (\displayId -> spawnPipe $ "/usr/bin/xmobar " ++ (if displayId == 0 then "" else "-p Top -x " ++ (show displayId)) ++ " ~/.xmobarrc") [0..numDisplay-1]
    io (threadDelay (1 * 1000 * 1000))
--    spawn "xrandr  --verbose --output eDP-1 --off; xrandr  --verbose --output eDP-1 --auto"
--    spawn "sleep 1; gnome-session; xinput --set-prop 12 'libinput Accel Speed' 0.791367"
    spawn "gnome-screensaver"
    spawn "pulseaudio --start"
    spawn "killall dunst"
--    spawn "xrandr --verbose --output DP-3 --rotate right"


    xmonad $ gnomeConfig -- defaultConfig
        { manageHook = myManageHookAll
        , layoutHook =  myLayoutHookAll
        , logHook = withWindowSet (\s -> L.foldl (>>) def (map (\(i, xmproc) -> dynamicLogWithPP (multiScreenXMobarPP s i xmproc)) (L.zip [0..(L.length xmprocs)] xmprocs)))
        , handleEventHook = handleEventHook gnomeConfig <+> docksEventHook <+> (\e -> do
            logCurrentMouseLocation
            return (All True))
        , startupHook = startupHook gnomeConfig <+> docksStartupHook <+> configureMouse
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
--        , borderWidth = 4
        , borderWidth = 4
--        , normalBorderColor  = "#587993" -- Java blue
--        , focusedBorderColor = "#e76f00" -- Java orange
        , normalBorderColor  = blue -- NieR Automata normal
        , focusedBorderColor = red -- NieR Automata forcused
--        , normalBorderColor  = "#b4af9a" -- NieR Automata normal
--        , focusedBorderColor = "#686458" -- NieR Automata forcused
        , focusFollowsMouse = False -- マウスの移動でフォーカスが映らないように
        , clickJustFocuses = False
        , XMonad.Core.workspaces = myWorkspaces
        } `additionalKeys`
        [
          ((mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock") -- Lock
        , ((mod4Mask .|. shiftMask, xK_s), spawn "systemctl suspend") -- Lock & Suspend
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_l), io (exitWith ExitSuccess)) -- Logout
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_s), spawn "/usr/lib/indicator-session/gtk-logout-helper --shutdown") -- Shutdown

        , ((controlMask, xK_Print), spawn "gnome-screenshot -c")
        , ((0, xK_Print), spawn "gnome-screenshot")
        , ((mod4Mask, xK_r), withWindowSet (\ws -> do
                                                     let sid = W.screen $ W.current ws
                                                     viewScreen 0 >> refresh >> docksStartupHook >> viewScreen sid)) -- rescreen >> 
        , ((mod4Mask, xK_q), viewScreen 0 >> spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

        , ((mod4Mask .|. shiftMask, xK_e), spawn "nautilus")

        , ((mod4Mask, xK_Return), myNamedScratchpadAction "mainterm")
        , ((mod4Mask, xK_F8), myNamedScratchpadAction "rhythmbox")
        , ((mod4Mask, xK_F9), myNamedScratchpadAction "艦これ")
        , ((mod4Mask, xK_F10), myNamedScratchpadAction "bunnaru")
        , ((mod4Mask, xK_F11), myNamedScratchpadAction "term1")
        , ((mod4Mask, xK_F12), myNamedScratchpadAction "term2")
        , ((mod4Mask .|. controlMask, xK_F11), myNamedScratchpadAction "jshell1")
        , ((mod4Mask .|. controlMask, xK_F12), myNamedScratchpadAction "jshell2")
        , ((mod4Mask, xK_bracketleft), myNamedScratchpadAction "term1")
        , ((mod4Mask, xK_bracketright), myNamedScratchpadAction "term2")
        , ((mod4Mask .|. controlMask, xK_bracketleft), myNamedScratchpadAction "jshell1")
        , ((mod4Mask .|. controlMask, xK_bracketright), myNamedScratchpadAction "jshell2")

        , ((mod4Mask .|. controlMask, xK_F7), toggleScrachpadAction $ L.reverse myScratchpads)
        , ((mod4Mask .|. controlMask, xK_F8), showOrHideScratchpads myScratchpads True)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_F8), showOrHideScratchpads myScratchpads False)
        , ((mod4Mask .|. controlMask, xK_F9), toggleScrachpadAction myScratchpads)

        , ((mod4Mask, xK_backslash), launchIntelliJTerminal)

        , ((mod4Mask, xK_g), selectSearchBrowser "/usr/bin/vivaldi" google)

        , ((mod4Mask, xK_d), sendMessage NextLayout)
        -- Full screen
        , ((mod4Mask, xK_f), sendMessage ToggleLayout)

        -- 水平のサイズ変更
        , ((mod4Mask, xK_i), sendMessage MirrorExpand)
        , ((mod4Mask, xK_m), sendMessage MirrorShrink)

        -- Arrow Keys
        -- フォーカスの移動
        , ((mod4Mask, xK_Up), windows W.focusUp)
        , ((mod4Mask, xK_Left), prevWS')
        , ((mod4Mask, xK_Down), windows W.focusDown)
        , ((mod4Mask, xK_Right), nextWS')

        -- スワップ
        , ((mod4Mask .|. shiftMask, xK_Up), windows W.swapUp)
        , ((mod4Mask .|. shiftMask, xK_Left), windows W.swapUp)
        , ((mod4Mask .|. shiftMask, xK_Down), windows W.swapDown)
        , ((mod4Mask .|. shiftMask, xK_Right), windows W.swapDown)

        -- ワーススペースの移動
        , ((mod4Mask .|. controlMask, xK_Up), prevWS')
        , ((mod4Mask .|. controlMask, xK_Left), prevWS')
        , ((mod4Mask .|. controlMask, xK_Down), nextWS')
        , ((mod4Mask .|. controlMask, xK_Right), nextWS')

        -- ワーススペース間のスワップ
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up), shiftToPrevWS' >> prevWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Left), shiftToPrevWS' >> prevWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down), shiftToNextWS' >> nextWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Right), shiftToNextWS' >> nextWS')

        -- j/k key
        -- ワークスペースの移動
        , ((mod4Mask .|. controlMask, xK_j), nextWS')
        , ((mod4Mask .|. controlMask, xK_k), prevWS')

        -- ワークスペース間のスワップ
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_j), shiftToNextWS' >> nextWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_k), shiftToPrevWS' >> prevWS')

        -- スクリーンの移動
        , ((mod4Mask .|. mod1Mask, xK_j), nextScreen >> moveMouseToLastPosition)
        , ((mod4Mask .|. mod1Mask, xK_k), prevScreen >> moveMouseToLastPosition)
        , ((mod4Mask, xK_space), nextScreen >> moveMouseToLastPosition)
        , ((mod4Mask .|. shiftMask, xK_space), prevScreen >> moveMouseToLastPosition)

        , ((mod4Mask, xK_w), goToSelected' hidpiGSConfig)
        , ((mod4Mask .|. shiftMask, xK_w), gridselectWorkspace (hidpiGSConfig {gs_cellwidth = 80}) W.view)

        , ((mod4Mask, xK_p), spawn $ "dmenu_run -nb '" ++ white ++ "' -nf '" ++ black ++ "' -sb '" ++ black ++ "' -p '❖'")
        , ((mod4Mask .|. shiftMask, xK_p), spawn "gmrun")
        , ((mod4Mask, xK_e), spawnSelected hidpiGSConfig applications)
--        , ((mod4Mask, xK_s), scratchpadSelected hidpiGSConfig myScratchpads)

        -- CopyWindow
        , ((mod4Mask, xK_a), windows copyToAll)
        , ((mod4Mask .|. shiftMask, xK_a), killAllOtherCopies)
        , ((mod4Mask, xK_z), showAllWindow)

        -- Screenshot
        , ((mod4Mask, xK_s), spawn "sh ~/.xmonad/screenshot.sh")
        , ((mod4Mask .|. controlMask, xK_s), spawn "sh ~/.xmonad/screenshot.sh -a")
        ] `additionalKeysP`
        [
        -- 輝度・ボリューム周り
          ("<XF86MonBrightnessDown>", spawn "sh ~/.xmonad/bright_down.sh")
        , ("<XF86MonBrightnessUp>", spawn "sh ~/.xmonad/bright_up.sh")
        , ("<XF86KbdBrightnessDown>", spawn "sh ~/.xmonad/kbd_bright_down.sh")
        , ("<XF86KbdBrightnessUp>", spawn "sh ~/.xmonad/kbd_bright_up.sh")
        , ("<XF86AudioLowerVolume>", spawn "sh ~/.xmonad/audio_down.sh")
        , ("<XF86AudioRaiseVolume>", spawn "sh ~/.xmonad/audio_up.sh")
        , ("M4-<XF86AudioLowerVolume>", spawn "sh ~/.xmonad/audio_prev.sh")
        , ("M4-<XF86AudioRaiseVolume>", spawn "sh ~/.xmonad/audio_next.sh")
        , ("<XF86AudioMute>",        spawn "sh ~/.xmonad/audio_mute.sh")
--        , ("<XF86AudioLowerVolume>", setMute(False) >> lowerVolume 3 >> return ())
--        , ("<XF86AudioRaiseVolume>", setMute(False) >> raiseVolume 3 >> return ())
--        , ("<XF86AudioMute>",        setMute(False) >> setVolume 50   >> return ()) -- toggleMuteで問題がなければそうすると良いです。
        , ("<XF86LaunchA>", showOrHideScratchpads myScratchpads False)
        , ("<XF86LaunchB>", showOrHideScratchpads myScratchpads True)
        ] `removeKeys`
        [
          (mod4Mask .|. shiftMask, xK_q)
--        , (mod4Mask, xK_q)
        ] `additionalMouseBindings` [
          ((mod4Mask .|. controlMask, button1), \w -> focus w >> mouseResizeWindow w
                                                              >> windows W.shiftMaster)
        ] `additionalKeys` [
          ((mod4Mask .|. m, k), f i)
            | (i, k) <- zip originalWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
            , (f, m) <- [(greedyViewToWorkspace, 0), (shiftToWorkspace, shiftMask)]
        ] `additionalKeys` [
          ((mod1Mask .|. m, k), f i)
            | (i, k) <- zip workspaceFamilies $ [xK_1 .. xK_9] ++ [xK_0]
            , (f, m) <- [
              (\family -> submap . M.fromList $
                         [ ((0, subkey), greedyViewToFamilyWorkspace family workspace)
                               | (workspace, subkey)  <- zip originalWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
                         ], 0)
            , (\family -> submap . M.fromList $
                          [ ((0, subkey), shiftToFamilyWorkspace family workspace)
                                | (workspace, subkey) <- zip originalWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
                          ], shiftMask)
            ]
        ]

-- Libraries

------------------------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------------------------
onTop = (customFloating $ W.RationalRect 0 0.02 1 0.48)
onBottom = (customFloating $ W.RationalRect 0 0.5 1 0.5)

javaHome = "~/bin/jdk-10.0.1/"
jshellPath = javaHome ++ "/bin/jshell"

terminalScratchpad :: String -> Maybe String -> ManageHook -> NamedScratchpad
terminalScratchpad name execMaybe manageHook =
    NS name
       ("/usr/lib/gnome-terminal/gnome-terminal-server" ++
           " --app-id bitter_fox.xmonad." ++ name ++
           " --name=" ++ name ++ " --class=" ++ name ++
           " & gnome-terminal --app-id bitter_fox.xmonad." ++ name ++
           (case execMaybe of
              Just exec -> " -e " ++ exec
              Nothing -> ""
           )
       )
       (appName =? name)
       manageHook

myScratchpads :: [NamedScratchpad]
myScratchpads = [
    terminalScratchpad "mainterm" Nothing
           (customFloating $ W.RationalRect 0.01 0.03 0.98 0.96)
  , terminalScratchpad "term1" Nothing onTop
  , terminalScratchpad "term2" Nothing onBottom
  , terminalScratchpad "jshell1" (Just jshellPath) onTop
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
           (customFloating $ W.RationalRect 0 0.02 1 0.98)
 ]

myScratchpadsManageHook = namedScratchpadManageHook myScratchpads

myNamedScratchpadAction = myNamedScratchpadActionInternal myScratchpads

myNamedScratchpadActionInternal scratchpads n =
    namedScratchpadAction scratchpads n
    >> withWindowSet (\s ->
         case (W.peek s, L.find isSameName scratchpads) of
           (Just w, Just ns) -> do
             hook <- runQuery (hook ns) w
             isTarget <- runQuery (query ns) w
             case isTarget of
               True -> windows(\ws -> appEndo hook ws)
               _ -> return ()
           _ -> return ()
       )
  where
    isSameName = \ns -> n == (name ns)

myNamedScratchpadActionMaybe mns = case mns of
    Just ns -> myNamedScratchpadAction $ name ns
    _ -> return ()

runScratchpadAction scratchpad = myNamedScratchpadActionInternal [scratchpad] $ name scratchpad

toggleScrachpadAction scratchpads =
    withWindowSet (\s ->
      case W.peek s of
        Just w -> do
          (cur, next) <- findCurrentAndNextScrachpadOf scratchpads w
          myNamedScratchpadActionMaybe cur >> myNamedScratchpadActionMaybe next
        _ -> myNamedScratchpadAction $ name (head scratchpads)
    )

findCurrentAndNextScrachpadOf nss w =
    findIt (cycle ((asMaybe nss) ++ [Nothing]))
  where
    asMaybe = \l -> map Just l
    findIt = \l ->
               let
                   h = head l
                   t = tail l
               in
                 case h of
                   Just ns -> do
                          matched <- runQuery (query ns) w
                          if matched then
                              return (h, head t)
                          else
                              findIt t
                   _ -> return (h, head t)
------------------------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- Evacuation
------------------------------------------------------------------------------------------
data MaybeWindowLocationMap = MaybeWindowLocationMap (M.Map Window (W.RationalRect, (Rational, Rational))) deriving Typeable
instance ExtensionClass MaybeWindowLocationMap where
  initialValue = MaybeWindowLocationMap M.empty

showOrHideScratchpads scratchpads show =
    withWindowSet(\s -> do
--      filtered <- windowsMatchScratchpads s
      toMoveActions s (currentWindows s)
--      toMoveActions s filtered
    )
  where
    acceleration = 0.001
    delay = 5
    toShowActions = \s ws -> do
                      MaybeWindowLocationMap evacuateds <- XS.get
                      let
                        currents = W.floating s;
                        toBeShowns = mapMaybe (\w -> do
                          current <- M.lookup w currents
                          (W.RationalRect x y width height, (_x, _y))  <- M.lookup w evacuateds
                          Just (w, current, (x, y))
                          ) ws
--                      io $ appendFile "/tmp/xmonad.debug" "show\n"
--                      io $ appendFile "/tmp/xmonad.debug" $ TS.show toBeShowns
                      dynamicMoving toBeShowns acceleration delay
                      XS.put $ MaybeWindowLocationMap $ L.foldl (\m (w, c, t) -> M.delete w m) evacuateds toBeShowns
    toHideActions = \s ws -> do
                      MaybeWindowLocationMap evacuateds <- XS.get
                      let
                        currents = W.floating s
                        toBeEvacuateds = mapMaybe (\w -> do
                          r <- M.lookup w currents
                          let W.RationalRect x y width height = r
                          case M.lookup w evacuateds of
                            Nothing -> Just (w, r)
                            Just (original, e)
                              | ((x, y) == e) -> Nothing -- (r == original) || 
                              | otherwise -> Just (w, r)
                          ) ws
--                      io $ appendFile "/tmp/xmonad.debug" "hide\n"
--                      io $ appendFile "/tmp/xmonad.debug" $ TS.show toBeEvacuateds
                      newLocationMap <- evacuateWindowsLikeMac toBeEvacuateds acceleration delay
                      let
                        evacuatedWindowLocations = mapMaybe (\(w, c) -> do
                          newLocation <- M.lookup w newLocationMap
                          Just (w, c, newLocation)
                          ) toBeEvacuateds
                      evacuatedLocations <- return (mapMaybe (\(w, c) -> do
                        W.RationalRect x y width height <- M.lookup w $ W.floating s
                        Just (w, c, (x, y))
                        ) toBeEvacuateds)
                      XS.put $ MaybeWindowLocationMap $ L.foldl (\m (w, c, t) -> M.insert w (c, t) m) evacuateds evacuatedWindowLocations
    toMoveActions = if show then toShowActions else toHideActions
    currentWindows = maybe [] W.integrate . W.stack . W.workspace . W.current -- ws -> [Window]
    queries = map query scratchpads
    isMatchScratchpads = \w ->
        L.foldl (<||>) (return False) (L.map (\ns -> runQuery (query ns) w) scratchpads)
    windowsMatchScratchpads = \s -> filterM isMatchScratchpads $ currentWindows s -- WindowSet -> X [Window]
--    hideHook = \w -> return (customFloating $ W.RationalRect (-1000) (-1000) 0 0)
--    toHideActions = \s ws -> dynamicMoving (mapMaybe (\(f, w) ->
--                      do
--                        fromRect <- M.lookup w (W.floating s)
--                        let W.RationalRect x y width height = fromRect
--                        Just (f w fromRect)
--                      ) (L.zip hideActionFactories ws)) 0.001 10
--    hideActionFactories = cycle [toLeft, toCenter, toRight]
--    toCenter = \w rect -> let W.RationalRect x y width height = rect in (w, rect, (x, 0.03-height))
--    toLeft = \w rect -> let W.RationalRect x y width height = rect in (w, rect, (0.03-width, 0.03-height))
--    toRight = \w rect -> let W.RationalRect x y width height = rect in (w, rect, (0.97, (0.03-height)))
--    toCenter = \w rect -> let W.RationalRect x y width height = rect in (w, rect, (x, 0.99))
--    toLeft = \w rect -> let W.RationalRect x y width height = rect in (w, rect, ((0.01-width), 0.99))
--    toRight = \w rect -> let W.RationalRect x y width height = rect in (w, rect, (0.99, 0.99))

-- toggleFloatingWindowsEvacuation :: X ()
-- toggleFloatingWindowsEvacuation =

evacuateWindowsLikeMac :: [(Window, W.RationalRect)] -> Rational -> Int-> X (M.Map Window (Rational, Rational))
evacuateWindowsLikeMac l acceleration delay = do
    let withTo = L.map computeTo l
    dynamicMoving withTo acceleration delay
    return $ L.foldl (\m (w, c, t) -> M.insert w t m) M.empty withTo
  where
    computeTo = \(w, r) -> (w, r, to r)
    delta = 0.02
    topX = \(W.RationalRect x y w h) -> delta-w
    topY = \(W.RationalRect x y w h) -> delta+0.02-h
    bottomX = \(W.RationalRect x y w h) -> 1-delta
    bottomY = \(W.RationalRect x y w h) -> 1-delta
    dx = \(W.RationalRect x y w h) -> x + (w/2)
    dy = \(W.RationalRect x y w h) -> y + (h/2)
    fx = \r x -> if (dx r) == 0.5 then let W.RationalRect _x _y w h = r in _y else
          let
            a = ((dy r)-0.5)/((dx r)-0.5)
            W.RationalRect _x _y w h = r
          in
            a*(x+(w/2))+0.5*(1-a)-(h/2)
    fy = \r y -> if (dy r) == 0.5 then let W.RationalRect _x _y w h = r in _x else
          let
            a = ((dx r)-0.5)/((dy r)-0.5)
            W.RationalRect _x _y w h = r
          in
            a*(y+(h/2))+0.5*(1-a)-(w/2)
    leftTop     = 0
    rightTop    = 1
    leftBottom  = 2
    rightBottom = 3
    whereIs = \r ->
                if (dy r) <= 0.5 then -- Top
                  if (dx r) <= 0.5 then leftTop else rightTop
                else
                  if (dx r) <= 0.5 then leftBottom else rightBottom
    to = \r -> let W.RationalRect x y w h = r in
--                 l = whereIs r
--               in
--                 if l == leftTop then
--                   let _y = fx r $ topX r in
--                     if _y+h >= 0 then (topX r, _y) else (fy r $ topY r, topY r)
--                 else if l == rightTop then
--                   let _y = fx r $ bottomX r in
--                     if _y+h >= 0 then (bottomX r, _y) else (fy r $ topY r, topY r)
--                 else if l == leftBottom then
--                   let _y = fx r $ topX r in
--                     if (_y+h >= 0) && (_y < bottomY r) then (topX r, _y) else (fy r $ bottomY r, bottomY r)
--                 else
--                   let _y = fx r $ bottomX r in
--                     if (_y+h >= 0) && (_y < bottomY r) then (bottomX r, _y) else (fy r $ bottomY r, bottomY r)
          if ((dx r) == 0.5) || (abs (((dy r) - 0.5) / ((dx r) - 0.5)) > 1) then
            if (dx r) == 0.5 then (x, if (dy r) <= 0.5 then topY r else bottomY r) else
            if (dy r) <= 0.5 then
              (fy r $ topY r, topY r)
            else
              let _x = fy r $ bottomY r in
                if (_x+w > 0) && (_x < 1) then
                    (_x, bottomY r)
                else
                  if (dx r) <= 0.5 then (topX r, fx r $ topX r)
                  else (bottomX r, fx r $ bottomX r)
          else
            if (dy r) == 0.5 then (if (dx r) <= 0.5 then topX r else bottomX r, y) else
            if (dx r) <= 0.5 then
              let _y = fx r $ topX r in
                if (_y+h > 0) && (_y < 1) then
                  (topX r, _y)
              else
                if (dy r) <= 0.5 then (fy r $ topY r, topY r)
                else (fy r $ bottomY r, bottomY r)
            else
              (bottomX r, fx r $ bottomX r)

dynamicMoving :: [(Window, W.RationalRect, (Rational, Rational))] -> Rational -> Int -> X ()
dynamicMoving l acceleration delay =
    moving (directionize l) 0
  where
    directionize = map directionizeMapper
    directionizeMapper = \(w, r, (toX, toY)) ->
                           let
                             W.RationalRect x y width height = r
                             dirX = if x <= toX then right else left
                             dirY = if y <= toY then down else up
                           in
                             (w, r, (toX, toY), (dirX, dirY))
    up = -1
    down = 1
    left = -1
    right = 1
    checkDoneAll = \l -> L.foldr (&&) True $ L.map checkDone l
    checkDone = \t -> (&&) (doneX t) (doneY t)
    doneX = \(w, W.RationalRect x y width height, (toX, toY), (dirX, dirY)) -> ((toX-x)*dirX)<=0
    doneY = \(w, W.RationalRect x y width height, (toX, toY), (dirX, dirY)) -> ((toY-y)*dirY)<=0
    moving = \l speed ->
               if checkDoneAll l then
                 moveAll (L.map forceToLocation l)
               else
                 let
                   newList = L.map (\t ->
                     let
                       (w, W.RationalRect x y width height, (toX, toY), (dirX, dirY)) = t
                       k = 1
                       diffX = abs(x-toX)
                       diffY = abs(y-toY)
                       newX = if doneX t then toX else x+(speed*k*dirX)*(diffX/(diffX+diffY))
                       newY = if doneY t then toY else y+(speed*k*dirY)*(diffY/(diffX+diffY))
                     in
                       (w, W.RationalRect newX newY width height, (toX, toY), (dirX, dirY))) l
                 in
                   io (if delay == 0 then return () else threadDelay delay)
                   >> moveAll (L.map (
                     \t -> if checkDone t then forceToLocation t else
                       let (w, to, (toX, toY), (dirX, dirY)) = t in (w, to)
                     ) newList
                   )
                   >> moving newList (speed+acceleration)
    forceToLocation = \(w, W.RationalRect x y width height, (toX, toY), (dirX, dirY)) -> (w, W.RationalRect toX toY width height)
    moveAll = \l -> windows (\s ->
                L.foldl (\ss (w, r) -> W.float w r ss) s l
              )
    move = \(w, newRect) -> windows $ W.float w newRect
------------------------------------------------------------------------------------------
-- Evacuation
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- WorkspaceFamily
------------------------------------------------------------------------------------------
notSP :: X (WindowSpace -> Bool)
notSP = return $ ("NSP" /=) . W.tag
currentWorkspaceFamily currentFamily = return (\ws -> L.isSuffixOf ("_" ++ currentFamily) $ W.tag ws)

currentFamilyId ws = toFamilyId $ W.tag $ W.workspace $ W.current ws
toFamilyId = drop 2
currentWorkspaceId ws = toWorkspaceId $ W.tag $ W.workspace $ W.current ws
toWorkspaceId id = [ head id ]
nextWS' :: X ()
nextWS' = withWindowSet(\s -> do
            let wsid = W.tag $ W.workspace $ W.current s
            if (wsid == "NSP") then
                return ()
            else
                moveTo Next $ WSIs $ currentWorkspaceFamily $ toFamilyId wsid
          )
prevWS' :: X ()

prevWS' = withWindowSet(\s -> do
            let wsid = W.tag $ W.workspace $ W.current s
            if (wsid == "NSP") then
                return ()
            else
                moveTo Prev $ WSIs $ currentWorkspaceFamily $ toFamilyId wsid
          )

shiftToNextWS' :: X()
shiftToNextWS' = shiftTo Next (WSIs notSP)
shiftToPrevWS' :: X()
shiftToPrevWS' = shiftTo Prev (WSIs notSP)

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
    io $ appendFile "/tmp/xmonad.debug" $ workspaceId ++ "_" ++ familyId
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
    io $ appendFile "/tmp/xmonad.debug2" $ workspaceId ++ "_" ++ familyId
    windows (W.shift (workspaceId ++ "_" ++ familyId))

multiScreenXMobarPP windowSet screenId xmproc = xmobarPP
                        { ppOutput = \t -> hPutStrLn xmproc $ (fallbackIfNoScreen (currentOfScreenId True) windowSet screenId) ++ " | " ++ t
                        , ppTitle = \t -> ""
                        , ppSep             = " | "
                        , ppExtras = [ titleOfScreenId windowSet screenId ]
                        , ppCurrent = fallbackIfNoScreen (showOnlyWorkspaceFor $ currentOfScreenId False) windowSet screenId
                        , ppVisible = fallbackIfNoScreen (showOnlyWorkspaceFor visibleOfScreenId) windowSet screenId
                        , ppHidden = fallbackIfNoScreen (showOnlyWorkspaceFor $ \ws -> \sid -> ppHidden xmobarPP) windowSet screenId
                        , ppLayout = \t -> layoutOfScreenId windowSet screenId
                        , ppSort = fmap (. namedScratchpadFilterOutWorkspace) $ ppSort xmobarPP
                        }

titleOfScreenId windowSet screenId =
    case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
      Just sc -> case ((W.stack (W.workspace sc)) >>= (\st -> Just (W.focus st))) of
                   Just w -> fmap (\nw -> Just (" " ++ (show nw) ++ " ")) $ getName w
--                   Just w -> fmap (\nw -> Just (xmobarColor (if ((W.screen (W.current windowSet)) == (W.screen sc)) then "#4E4B42" else "#D9D3BA") (if ((W.screen (W.current windowSet)) == (W.screen sc)) then "#D9D3BA" else "#4E4B42") (" " ++ (show nw) ++ " "))) (getName w)
                   Nothing -> def
      Nothing -> titleOfScreenId windowSet 0 -- optimize

layoutOfScreenId windowSet screenId =
    case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
      Just sc -> description . W.layout . W.workspace $ sc
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

visibleOfScreenId windowSet screenId wid =
    case L.find (\sc -> (W.screen sc) == S screenId) (W.visible windowSet) of
--      Just sc -> if (W.tag (W.workspace sc) == wid) then xmobarColor "#4E4B42" "#D9D3BA" (wrap " " " " wid) else wrap "a" "a" wid
      Just sc ->  xmobarColor black white $ wrap " " " " wid
      Nothing -> wrap "" "" wid

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

fallbackIfNoScreen f windowSet screenId =
    case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
      Just sc -> f windowSet screenId $ toFamilyId $ W.tag $ W.workspace $ sc
      Nothing -> f windowSet 0 (currentFamilyId windowSet)
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
logCurrentMouseLocation =
    withWindowSet (\ws ->
      do
        MousePosition maybeLastMousePosition <- XS.get
        xconf <- ask
        let maybeMousePos = mousePosition xconf
        if maybeLastMousePosition == maybeMousePos then
          def
        else
          do
            MousePositionMap lastMousePositions <- XS.get
            case maybeMousePos of
              Just (x, y) -> do
                               maybeScreen <- pointScreen x y
                               let screen = case maybeScreen of
                                              Just s -> if ((W.screen (W.current ws)) == W.screen s) then
                                                          Just $ W.screen s
                                                        else
                                                          Nothing
                                              Nothing -> Nothing
                               case screen of
                                 Just s ->
                                     (XS.put $ MousePositionMap $ M.insert s (x, y) lastMousePositions) >>
                                     (XS.put $ MousePosition $ maybeMousePos) -- >>
--                                     (spawn ("echo '" ++ (show lastMousePositions) ++ "' >> /tmp/xmonad.debug"))
--                                     (spawn ("echo '" ++ (show x) ++ " " ++ (show y) ++ "' >> " ++ mouseLogDir ++ "/" ++ s))
                                 Nothing -> def
              Nothing -> def
      )

moveMouseToLastPosition :: X ()
moveMouseToLastPosition =
    withWindowSet (\ws ->
      do
        MousePositionMap lastMousePositions <- XS.get
--        let s = nextScreenObjectOf ws
        let s = W.current ws
        case M.lookup (W.screen s) lastMousePositions of
          Just (x, y) -> moveMouseTo x y
          Nothing -> do
              let sd = W.screenDetail s
              let rect = screenRect sd
              let x = truncate $ (fromIntegral $ rect_x rect) + (fromIntegral $ rect_width rect) / 2
              let y = truncate $ (fromIntegral $ rect_y rect) + (fromIntegral $ rect_height rect) / 2
              moveMouseTo x y
        setMouseSpeedForScreen $ fromIntegral $ W.screen s
    )

moveMouseTo x y = runProcessWithInputAndWait "sh" ["-c", ("xdotool mousemove " ++ (show x) ++ " " ++ (show y))] "" (seconds 1) -- Can we move mouse within XMonad?

configureMouse = do
  moveMouseToLastPosition
  setMouseSpeedForScreen 0
  runProcessWithInputAndWait "sh" ["-c", "xinput --set-prop " ++ mouseDeviceId ++ " 'libinput Natural Scrolling Enabled' 1"] "" (seconds 1) -- Enable Natural scrooling
  runProcessWithInputAndWait "sh" ["-c", "xinput --set-prop " ++ mouseDeviceId ++ " 'libinput Click Method Enabled' 0 1"] "" (seconds 1) -- Right click on 2 fingure click

setMouseSpeedForScreen s = runProcessWithInputAndWait "sh" ["-c", "xinput --set-prop " ++ mouseDeviceId ++ " 'libinput Accel Speed' " ++ (mouseSpeed s)] "" (seconds 1)
mouseSpeed :: Int -> String
mouseSpeed n = ["0.791367", "1", "1"] !! n
mouseDeviceId = "12"

nextOf f e l@(x:_) = case dropWhile (\a -> f a /= f e) l of
                          (_:y:_) -> y
                          _       -> x
nextScreenObjectOf :: WindowSet -> (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
nextScreenObjectOf ws = nextOf W.screen (W.current ws) $ W.visible ws

mouseLogDir = "/tmp/xmonad/mouse"
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
--               activeColor = "black"
               activeColor = black
             , inactiveColor = white
             , urgentColor = "white"
--             , activeTextColor = "green"
             , activeTextColor = white
             , inactiveTextColor = black
             , urgentTextColor = "red"
             , activeBorderColor = black
             , inactiveBorderColor = white
             , urgentBorderColor = "pink"
             , decoHeight = 32
             , fontName = "xft:monospace-9:bold,Symbola-9:bold"
}

goToSelected' =
    withSelectedWindow' $ \w -> windows $ W.focusWindow w

-- | Like `gridSelect' but with the current windows and their titles as elements
gridselectWindow' :: GSConfig Window -> X (Maybe Window)
gridselectWindow' gsconf = windowMap' >>= gridselect gsconf

-- | Brings up a 2D grid of windows in the center of the screen, and one can
-- select a window with cursors keys. The selected window is then passed to
-- a callback function.
withSelectedWindow' :: (Window -> X ()) -> GSConfig Window -> X ()
withSelectedWindow' callback conf = do
    mbWindow <- gridselectWindow' conf
    case mbWindow of
        Just w -> callback w
        Nothing -> return ()

windowMap' :: X [(String,Window)]
windowMap' = do
    ws <- gets windowset
    wins <- mapM keyValuePair (foldr (++) [] $ map (W.integrate' . W.stack) $ filter (\w -> "NSP" /= W.tag w) $ W.workspaces ws)
    return wins
 where keyValuePair w = flip (,) w `fmap` decorateName' w

decorateName' :: Window -> X String
decorateName' w = do
  name <- getName' w
  clazz <- getClass' w
  workspace <- getWorkspace' w
  return ("[" ++ workspace ++ "] " ++ clazz ++ " : " ++ name)

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
-- IntelliJExternalTerminal
------------------------------------------------------------------------------------------
intelliJScrachpad :: String -> String -> ManageHook -> NamedScratchpad
intelliJScrachpad name workingDir manageHook =
    NS name
       ("/usr/lib/gnome-terminal/gnome-terminal-server" ++
           " --app-id bitter_fox.xmonad.intellij." ++ name ++
           " --name=bitter_fox.xmonad.intellij." ++ name ++ " --class=" ++ "intellij-terminal" ++
           " & gnome-terminal --app-id bitter_fox.xmonad.intellij." ++ name ++
           " --working-directory=" ++ workingDir
       )
       (appName =? ("bitter_fox.xmonad.intellij." ++ name))
       manageHook

launchIntelliJTerminal :: X ()
launchIntelliJTerminal =
    withFocused (\w -> do
      cname <- runQuery className w
      if cname == "jetbrains-idea" then do
          t <- runQuery title w
          case (parse intelliJInfo "/tmp/hoge" t) of
            Left e -> return ()
            Right [project, dir] -> runScratchpadAction $ intelliJScrachpad (project ++ ".onBottom") (extractHomeDirectory dir) onBottom
      else do
          aname <- runQuery appName w
          if L.isPrefixOf "bitter_fox.xmonad.intellij." aname then do
             let name = L.drop (L.length "bitter_fox.xmonad.intellij.") aname
             runScratchpadAction $ intelliJScrachpad name homeDirectory onBottom
          else return ()
    )

intelliJInfo :: Parser [String]
intelliJInfo = do
  project <- many (noneOf [' '])
  char ' '
  char '['
  dir <- many (noneOf [']'])
  char ']'
  return [project, dir]

homeDirectory = "/home/bitterfox"
extractHomeDirectory path =
    if L.head path == '~' then
        homeDirectory ++ L.tail path
    else path
------------------------------------------------------------------------------------------
-- IntelliJExternalTerminal
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
viewScreen sid = do
  mws <- screenWorkspace sid
  case mws of
    Nothing -> return ()
    Just ws -> windows $ W.view ws
