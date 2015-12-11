import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
import XMonad.Actions.GridSelect
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
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.Exit
import System.IO
import qualified Data.Map as M

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

    spawn "sleep 10 ; killall trayer ; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 20 --widthtype percent --transparent false --tint 0x000000 --height 17" -- gnome-sound-appletのアイコンが黒一色でない場合は--transparent trueにすると統一感があっていいです。 -- GNOMEのトレイを起動 -- XXX(sleep 2): #6: Trayer broken with nautilus

    spawn "wmname LG3D"

    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $
                       toggleLayouts (renamed [Replace "□"] $ noBorders Full)
 $                       ((renamed [Replace "├"] $ myLayout) ||| (renamed [Replace "┬"] $ Mirror myLayout)) -- tall, Mirror tallからFullにトグルできるようにする。(M-<Sapce>での変更はtall, Mirror tall)
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 110
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
        , ((mod4Mask .|. controlMask, xK_Up), prevWS)
        , ((mod4Mask .|. controlMask, xK_Left), prevWS)
        , ((mod4Mask .|. controlMask, xK_Down), nextWS)
        , ((mod4Mask .|. controlMask, xK_Right), nextWS)

        -- ワーススペース間のスワップ
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up), shiftToPrev >> prevWS)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down), shiftToNext >> nextWS)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)

        -- j/k key
        -- ワークスペースの移動
        , ((mod4Mask .|. controlMask, xK_j), nextWS)
        , ((mod4Mask .|. controlMask, xK_k), prevWS)

        -- ワークスペース間のスワップ
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_j), shiftToNext >> nextWS)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_k), shiftToPrev >> prevWS)

        , ((mod4Mask, xK_w), goToSelected defaultGSConfig)
        , ((mod4Mask .|. shiftMask, xK_w), gridselectWorkspace defaultGSConfig W.view)

        , ((mod4Mask, xK_e), spawnSelected defaultGSConfig ["firefox", "nautilus", "gimp", "emacs", "gnome-terminal", "gnome-control-center", "libreoffice", "~/bin/netbeans-8.0.1/bin/netbeans", "~/bin/idea-IC-139.225.3/bin/idea.sh"])
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

