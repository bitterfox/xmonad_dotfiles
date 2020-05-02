. ~/.xmonad/color.sh
speed=`cat /sys/devices/platform/applesmc.768/fan1_output`

text="🌀$speed"

if [ `echo "4000 <= $speed" | bc` = 1 ]; then
    echo -n "<fc=$white,$red>$text</fc>"
elif [ `echo "2000 >= $speed" | bc` = 1 ]; then
    echo -n "<fc=$white,$blue>$text</fc>"
else
    echo -n $text
fi
