. ~/.xmonad/color.sh
speed1=`cat /sys/devices/virtual/hwmon/hwmon2/fan1_input`
speed2=`cat /sys/devices/virtual/hwmon/hwmon2/fan2_input`

text="🌀$speed1,$speed2"

if [ `echo "7000 <= $speed1" | bc` = 1 ]; then
    echo -n "<fc=$white,$red>$text</fc>"
elif [ `echo "5000 >= $speed1" | bc` = 1 ]; then
    echo -n "<fc=$brightBlue,$black>$text</fc>"
else
    echo -n $text
fi
