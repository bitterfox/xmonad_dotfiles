. $(dirname $0)/color.sh

fan_device_dir="/sys/devices/virtual/hwmon/hwmon2"

if [ -d "$fan_device_dir" ]; then
    speed1=`cat $fan_device_dir/fan1_input`
    speed2=`cat $fan_device_dir/fan2_input`
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
fi
