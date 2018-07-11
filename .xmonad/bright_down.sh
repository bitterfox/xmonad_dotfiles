xrandr --output eDP-1 --brightness $((`xrandr --verbose | grep Bri | sed -r 's/.*: ([0-9]+\.[0-9]*)$/\1/'` - 0.1))
