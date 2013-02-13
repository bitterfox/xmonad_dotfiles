import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO
import qualified Data.Map as M

tall = Tall 1 (3/100) (1/2)

main = do
    spawn "killall trayer ; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 10 --widthtype percent --transparent false --tint 0x000000 --height 20" -- gnome-sound-appletのアイコンが黒一色でない場合は--transparent trueにすると統一感があっていいです。 -- GNOMEのトレイを起動
--    spawn "gnome-power-manager"
    spawn "killall nm-applet ; nm-applet" -- ネット接続のアプレットを起動
    spawn "gnome-sound-applet" -- gnome-volume-control-applet? -- ボリューム変更のアプレットを起動
    spawn "nautilus --no-default-window --no-desktop" -- nautilusを起動(なんのために起動するのかわからない)
    spawn "dropbox start" -- dropboxを起動させて同期できるようにする
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ toggleLayouts (named "□" $ noBorders Full) ((named "├" $ tall) ||| (named "┬" $ Mirror tall)) -- tall, Mirror tallからFullにトグルできるようにする。(M-<Space>での変更はtall, Mirror tall)
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = 3
        , normalBorderColor  = "#5382a1" -- Java blue
        , focusedBorderColor = "#e76f00" -- Java orange
        , focusFollowsMouse = False -- マウスの移動でフォーカスが映らないように
        } `additionalKeys`
        [
          ((mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock") -- Lock
        , ((mod4Mask .|. shiftMask, xK_s), spawn "gnome-screensaver-command --lock ; dbus-send --print-reply --system --dest=org.freedesktop.UPower /org/freedesktop/UPower org.freedesktop.UPower.Suspend") -- Lock & Suspend
        , ((controlMask, xK_Print), spawn "gnome-screenshot -c")
        , ((0, xK_Print), spawn "gnome-screenshot")
        , ((mod4Mask, xK_r), refresh)

        -- Full screen
        , ((mod4Mask, xK_f), sendMessage ToggleLayout)

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
        ]`additionalKeysP`
        [
        -- ボリューム周り
          ("<XF86AudioLowerVolume>", setMute(False) >> lowerVolume 3 >> return ())
        , ("<XF86AudioRaiseVolume>", setMute(False) >> raiseVolume 3 >> return ())
        , ("<XF86AudioMute>",        setMute(False) >> setVolume 0   >> return ()) -- toggleMuteで問題がなければそうすると良いです。
        ]

