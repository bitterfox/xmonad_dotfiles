#xrandr --output eDP-1 --brightness `xrandr --verbose | grep Bri | head -n 1 | sed -r 's/.*: ([0-9]+\.[0-9]*)$/\1/' | xargs -i% echo "% + 0.1" | bc`

current=`pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --get-brightness`
max=`pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --get-max-brightness`
new=$(( current + max / 10 ))
if [ $new -gt $max ]; then
    new=$max
fi
pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --set-brightness $new
