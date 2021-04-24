. ~/.xmonad/color.sh

temp=`cat /sys/devices/platform/coretemp.0/hwmon/hwmon*/temp1_input | xargs -i% echo -e 'scale=1\n%/1000' | bc`

throttle_count=`cat /sys/devices/system/cpu/cpu0/thermal_throttle/package_throttle_count`

text="🌡$temp℃ ($throttle_count)"

if [ `echo "80 <= $temp" | bc` = 1 ]; then
    emergency
elif [ `echo "50 >= $temp" | bc` = 1 ]; then
    ok
fi
xmobar_echo $text
