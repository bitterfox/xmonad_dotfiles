. ~/.xmonad/color.sh

temp=`cat /sys/devices/platform/coretemp.0/hwmon/hwmon*/temp1_input | xargs -i% echo -e 'scale=1\n%/1000' | bc`

throttle_count=`cat /sys/devices/system/cpu/cpu0/thermal_throttle/package_throttle_count`

text="🌡$temp℃ ($throttle_count)"

if [ `echo "80 <= $temp" | bc` = 1 ]; then
    echo -n "<fc=$white,$red>$text</fc>"
elif [ `echo "50 >= $temp" | bc` = 1 ]; then
    echo -n "<fc=$brightBlue,$black>$text</fc>"
else
    echo -n $text
fi
