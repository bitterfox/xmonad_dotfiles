. ~/.xmonad/color.sh

temp=`cat /sys/devices/platform/applesmc.768/temp9_input | xargs -i% echo -e 'scale=1\n%/1000' | bc`

text="🌡$temp℃"

if [ `echo "70 <= $temp" | bc` = 1 ]; then
    echo -n "<fc=$white,$red>$text</fc>"
elif [ `echo "50 >= $temp" | bc` = 1 ]; then
    echo -n "<fc=$white,$blue>$text</fc>"
else
    echo -n $text
fi
