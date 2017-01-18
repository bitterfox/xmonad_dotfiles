import XMonad
import XMonad.Core
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
import XMonad.Actions.GridSelect
import XMonad.Actions.Search (selectSearchBrowser, google)
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Layout.ToggleLayouts
import XMonad.ManageHook
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import System.Exit
import System.IO
import qualified Data.List as L
import qualified Data.Map as M
import Data.Monoid
import Control.Monad (foldM, filterM)
import Control.Concurrent

onTop = (customFloating $ W.RationalRect 0 0.02 1 0.48)

myScratchpads :: [NamedScratchpad]
myScratchpads = [
    NS "mainterm"  "gnome-terminal --disable-factory --name=mainterm" (appName =? "mainterm")
        (customFloating $ W.RationalRect 0 0.02 1 0.98)
  , NS "term1"  "gnome-terminal --disable-factory --name=term1" (appName =? "term1")
        (customFloating $ W.RationalRect 0 0.02 1 0.48)
  , NS "term2"  "gnome-terminal --disable-factory --name=term2" (appName =? "term2")
        (customFloating $ W.RationalRect 0 0.5 1 0.5)
  , NS "bunnaru"  "google-chrome --new-window http://www.dmm.com/netgame/social/-/gadgets/=/app_id=798209/" (fmap (L.isInfixOf "文豪とアルケミスト - オンラインゲーム - DMM GAMES") title)
        (customFloating $ W.RationalRect 0 0.02 1 0.98)
 ]
myScratchpadsManageHook = namedScratchpadManageHook myScratchpads

myNamedScratchpadAction n =
    namedScratchpadAction myScratchpads n
    >> withWindowSet (\s ->
         case (W.peek s, L.find isSameName myScratchpads) of
           (Just w, Just ns) -> do
             isTarget <- runQuery (query ns) w
             hook <- runQuery (hook ns) w
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

showOrHideScratchpads scratchpads show =
    withWindowSet(\s -> do
      filtered <- windowsMatchScratchpads s
      L.foldl (>>) (return ())
        (L.map (hook s) filtered)
    )
  where
    showHook = \s w -> do
      nss <- filterM (\ns -> (runQuery (query ns) w)) scratchpads
      h <- runQuery (namedScratchpadManageHook nss) w
      windows (appEndo h)
--    hideHook = \w -> return (customFloating $ W.RationalRect (-1000) (-1000) 0 0)
    hideHook = \s w ->
                 let
                   toRect = W.RationalRect (-1) (-1) 0 0
                 in
                   case M.lookup w (W.floating s) of
                     Just fromRect -> dynamicMove w fromRect toRect (10) 0.05
                     _ -> return ()

    dynamicMove = \w fromRect toRect delay deltaY ->
                  let
                    W.RationalRect fx fy fw fh = fromRect
                    W.RationalRect tx ty tw th = toRect
                    newRect = W.RationalRect fx (fy-deltaY) fw fh
                    q = customFloating $ newRect
                  in
                    if fy > ty then do
                      h <- runQuery q w
                      io (threadDelay delay) >> windows (appEndo h)
                        >> dynamicMove w newRect toRect delay deltaY
                    else do
                      h <- runQuery (customFloating toRect) w
                      windows (appEndo h)

    hook = if show then showHook else hideHook
    currentWindows = maybe [] W.integrate . W.stack . W.workspace . W.current -- ws -> [Window]
    queries = map query scratchpads
    isMatchScratchpads = \w ->
        L.foldl (<||>) (return False) (L.map (\ns -> runQuery (query ns) w) scratchpads)
    windowsMatchScratchpads = \s -> filterM isMatchScratchpads $ currentWindows s -- WindowSet -> X [Window]

notSP :: X (WindowSpace -> Bool)
notSP = return $ ("NSP" /=) . W.tag

nextWS' :: X ()
nextWS' = moveTo Next (WSIs notSP)
prevWS' :: X ()
prevWS' = moveTo Prev (WSIs notSP)

tall = Tall 1 (3/100) (1/2)

main = do
    spawn "`sleep 3; xmodmap /home/bitter_fox/.xmodmap` &" -- for Mac keyboard
    spawn "ginn .wishes.xml" -- for Mac mouse

    spawn "nautilus --no-default-window" -- デスクトップを読み込む

    spawn "killall trayer ; sleep 2 ; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 20 --widthtype percent --transparent false --tint 0x000000 --height 17" -- gnome-sound-appletのアイコンが黒一色でない場合は--transparent trueにすると統一感があっていいです。 -- GNOMEのトレイを起動 -- XXX(sleep 2): #6: Trayer broken with nautilus

    spawn "gnome-settings-daemon" -- GNOME上での設定を反映させる
--    spawn "gnome-power-manager"
    spawn "killall nm-applet ; nm-applet" -- ネット接続のアプレットを起動
    spawn "gnome-sound-applet" -- gnome-volume-control-applet? -- ボリューム変更のアプレットを起動
    spawn "bluetooth-applet"
    spawn "dropbox start" -- dropboxを起動させて同期できるようにする
    spawn "sparkleshare restart"
    spawn "/opt/toggldesktop/TogglDesktop.sh"

    spawn "sleep 10 ; killall trayer ; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 20 --widthtype percent --transparent false --tint 0x000000 --height 17" -- gnome-sound-appletのアイコンが黒一色でない場合は--transparent trueにすると統一感があっていいです。 -- GNOMEのトレイを起動 -- XXX(sleep 2): #6: Trayer broken with nautilus

    spawn "wmname LG3D"

    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageHook defaultConfig
                       <+> manageDocks
                       <+> namedScratchpadManageHook myScratchpads
        , layoutHook = avoidStruts $
                       toggleLayouts (renamed [Replace "□"] $ noBorders Full)
 $                       ((renamed [Replace "├"] $ myLayout) ||| (renamed [Replace "┬"] $ Mirror myLayout)) -- tall, Mirror tallからFullにトグルできるようにする。(M-<Sapce>での変更はtall, Mirror tall)
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 60
                        , ppSort = fmap (. namedScratchpadFilterOutWorkspace) $ ppSort xmobarPP
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = 3
        , normalBorderColor  = "#587993" -- Java blue
        , focusedBorderColor = "#e76f00" -- Java orange
        , focusFollowsMouse = False -- マウスの移動でフォーカスが映らないように
        } `additionalKeys`
        [
          ((mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock") -- Lock
        , ((mod4Mask .|. shiftMask, xK_s), spawn "gnome-screensaver-command --lock ; dbus-send --print-reply --system --dest=org.freedesktop.UPower /org/freedesktop/UPower org.freedesktop.UPower.Suspend") -- Lock & Suspend
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_l), io (exitWith ExitSuccess)) -- Logout
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_s), spawn "/usr/lib/indicator-session/gtk-logout-helper --shutdown") -- Shutdown

        , ((controlMask, xK_Print), spawn "gnome-screenshot -c")
        , ((0, xK_Print), spawn "gnome-screenshot")
        , ((mod4Mask, xK_r), refresh)

        , ((mod4Mask .|. shiftMask, xK_e), spawn "nautilus")

        , ((mod4Mask, xK_Return), myNamedScratchpadAction "mainterm")
        , ((mod4Mask, xK_F10), myNamedScratchpadAction "bunnaru")
        , ((mod4Mask, xK_F11), myNamedScratchpadAction "term1")
        , ((mod4Mask, xK_F12), myNamedScratchpadAction "term2")

        , ((mod4Mask .|. controlMask, xK_F7), toggleScrachpadAction $ L.reverse myScratchpads)
        , ((mod4Mask .|. controlMask, xK_F8), showOrHideScratchpads myScratchpads True)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_F8), showOrHideScratchpads myScratchpads False)
        , ((mod4Mask .|. controlMask, xK_F9), toggleScrachpadAction myScratchpads)

        , ((mod4Mask, xK_g), selectSearchBrowser "/usr/bin/google-chrome-stable" google)

        -- Full screen
        , ((mod4Mask, xK_f), sendMessage ToggleLayout)

        -- 水平のサイズ変更
        , ((mod4Mask, xK_i), sendMessage MirrorExpand)
        , ((mod4Mask, xK_m), sendMessage MirrorShrink)

        -- Arrow Keys
        -- フォーカスの移動
        , ((mod4Mask, xK_Up), windows W.focusUp)
        , ((mod4Mask, xK_Left), windows W.focusUp)
        , ((mod4Mask, xK_Down), windows W.focusDown)
        , ((mod4Mask, xK_Right), windows W.focusDown)

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
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up), shiftToPrev >> prevWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Left), shiftToPrev >> prevWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down), shiftToNext >> nextWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Right), shiftToNext >> nextWS')

        -- j/k key
        -- ワークスペースの移動
        , ((mod4Mask .|. controlMask, xK_j), nextWS')
        , ((mod4Mask .|. controlMask, xK_k), prevWS')

        -- ワークスペース間のスワップ
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_j), shiftToNext >> nextWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_k), shiftToPrev >> prevWS')

        , ((mod4Mask, xK_w), goToSelected defaultGSConfig)
        , ((mod4Mask .|. shiftMask, xK_w), gridselectWorkspace defaultGSConfig W.view)

        , ((mod4Mask, xK_e), spawnSelected defaultGSConfig applications)
        ] `additionalKeysP`
        [
        -- ボリューム周り
          ("<XF86AudioLowerVolume>", setMute(False) >> lowerVolume 3 >> return ())
        , ("<XF86AudioRaiseVolume>", setMute(False) >> raiseVolume 3 >> return ())
        , ("<XF86AudioMute>",        setMute(False) >> setVolume 0   >> return ()) -- toggleMuteで問題がなければそうすると良いです。
        ] `removeKeys`
        [
          (mod4Mask .|. shiftMask, xK_q)
        ] `additionalMouseBindings`
        [
          ((mod4Mask .|. controlMask, button1), \w -> focus w >> mouseResizeWindow w
                                                              >> windows W.shiftMaster)
        ]

myLayout = (ResizableTall 1 (3/100) (1/2) [])

applications = [
 "google-chrome",
 "nautilus",
 "gimp",
 "emacs",
 "gnome-terminal",
 "gnome-control-center",
 "libreoffice",
 "~/bin/netbeans-8.0.1/bin/netbeans",
 "~/bin/idea-IC-139.225.3/bin/idea.sh"]
