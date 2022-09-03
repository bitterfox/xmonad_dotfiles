. $(dirname $0)/color.sh
speed1=`cat /sys/devices/virtual/hwmon/hwmon2/fan1_input`
speed2=`cat /sys/devices/virtual/hwmon/hwmon2/fan2_input`
if [ -n "$speed1" ]; then
    text="🌀$speed1"
    if [ -n "$speed2" ]; then
        text="$text,$speed2"
    fi
else
    exit 0
fi

if [ `echo "7000 <= $speed1" | bc` = 1 ]; then
    emergency
elif [ `echo "5000 >= $speed1" | bc` = 1 ]; then
    ok
fi
xmobar_echo " $text"
