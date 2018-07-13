xrandr --output eDP-1 --brightness `xrandr --verbose | grep Bri | head -n 1 | sed -r 's/.*: ([0-9]+\.[0-9]*)$/\1/' | xargs -i% echo "% - 0.1" | bc`
