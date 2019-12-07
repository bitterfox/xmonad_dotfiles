#xrandr --output eDP-1 --brightness `stdbuf -o0 xrandr --verbose | grep -m 1 Bri | sed -r 's/.*: ([0-9]+\.[0-9]*)$/\1 - 0.1/'| bc`

current=`pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --get-brightness`
max=`pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --get-max-brightness`
new=$(( current - max / 20 ))
fifth_percentile=$(( max / 20 ))
if [ $new -lt $fifth_percentile ]; then
  new=$(( current - max / 100 ))
fi
#echo $new >> /tmp/debug
pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --set-brightness $new
