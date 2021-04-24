. ~/.xmonad/color.sh

#current=`pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --get-brightness`
#max=`pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --get-max-brightness`

current="`cat "/sys/class/backlight/intel_backlight/brightness"`"
max="`cat "/sys/class/backlight/intel_backlight/max_brightness"`"
text="`echo "scale=2\nb=$current / $max*100\nscale=0\nb/1" | bc | xargs printf "%3d%%"`"

xmobar_echo "☀$text"
