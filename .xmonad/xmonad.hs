import System.IO
import System.Directory

import Control.Concurrent

import XMonad
import XMonad.Util.Run(runProcessWithInputAndWait, seconds)

import XMonad.Conf.MyXMobar
import XMonad.Conf.BitterfoxConfig

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
    xmprocs <- startXMobar

    spawn "gnome-screensaver"
    spawn "pulseeffects --gapplication-service"
    spawn "killall dunst"

    spawn "xrandr --output eDP-1 --brightness 1 --gamma 1.05:1.05:1.095"

    spawn "CM_MAX_CLIPS=10000 CM_DIR=$HOME CM_SELECTIONS=clipboard CM_IGNORE_WINDOW=xmonad.terminal.action.one.password clipmenud"

    spawn "~/.xmonad/system_scripts/bright/sync.sh"

--    spawn $ "echo '" ++ (show $ mkToggleInitial (single TitleTransformer) TitleTransformer $ myLayout) ++ "' >> /tmp/xmonad.debug.layout"
    config <- bitterfoxConfig xmprocs
    xmonad $ config
