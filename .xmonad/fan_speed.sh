. ~/.xmonad/color.sh
speed1=`cat /sys/devices/virtual/hwmon/hwmon2/fan1_input`
speed2=`cat /sys/devices/virtual/hwmon/hwmon2/fan2_input`

text="🌀$speed1,$speed2"

if [ `echo "7000 <= $speed1" | bc` = 1 ]; then
    emergency
elif [ `echo "5000 >= $speed1" | bc` = 1 ]; then
    ok
fi
xmobar_echo "$text"
