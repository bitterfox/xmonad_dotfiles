#xrandr --output eDP-1 --brightness `stdbuf -o0 xrandr --verbose | grep -m 1 Bri | sed -r 's/.*: ([0-9]+\.[0-9]*)$/\1 - 0.1/'| bc`

current=`pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --get-brightness`
max=`pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --get-max-brightness`
new=$(( current - max / 20 ))
if [ $new -lt 1 ]; then
    new=1
fi
pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --set-brightness $new
