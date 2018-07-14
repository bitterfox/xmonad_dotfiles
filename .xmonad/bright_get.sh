current=`pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --get-brightness`
max=`pkexec /usr/lib/gnome-settings-daemon/gsd-backlight-helper --get-max-brightness`
echo "scale=2\nb=$current / $max*100\nscale=0\nb/1" | bc | xargs -i! echo !%
