. ~/.xmonad/color.sh

temp=`cat /sys/devices/platform/coretemp.0/hwmon/hwmon3/temp1_input | xargs -i% echo -e 'scale=1\n%/1000' | bc`

text="🌡$temp℃"

if [ `echo "70 <= $temp" | bc` = 1 ]; then
    echo -n "<fc=$white,$red>$text</fc>"
elif [ `echo "50 >= $temp" | bc` = 1 ]; then
    echo -n "<fc=$brightBlue,$black>$text</fc>"
else
    echo -n $text
fi
