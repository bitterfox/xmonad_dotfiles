. ~/.xmonad/color.sh

bat="BAT0"

now=`cat /sys/class/power_supply/$bat/charge_now`
full=`cat /sys/class/power_supply/$bat/charge_full`

left=`echo "scale=1\n100 * $now / $full" | bc`
if [ `echo "100 < $left" | bc` = 1 ]; then
    left="100.0"
fi

status=`cat /sys/class/power_supply/$bat/status`

wat=`cat /sys/class/power_supply/$bat/current_now`
if [ "$status" = "Charging" ]; then
    time=`echo "scale=2\n($full - $now) / $wat" | bc | xargs printf "%1.2f"`
elif [ "$status" = "Discharging" ]; then
    time=`echo "scale=2\n$now / $wat" | bc | xargs printf "%1.2f"`
else
    time="0.0"
fi
time_hour=${time%.*}
time_min=`echo "($time - $time_hour) * 60"| bc`
time_min=${time_min%.*}
time_text=`printf "%02d:%02d" $time_hour $time_min`

text="🔋$left%($time_text)"

if [ "$status" = "Charging" ]; then
    if [ `echo "$left >= 95" | bc` = 1 ]; then
        text="<fc=$brightBlue,$black>$text</fc>"
    fi
elif [ "$status" = "Full" ]; then
    text="<fc=$brightBlue,$black>$text</fc>"
else
    if [ `echo "$left <= 15" | bc` = 1 ]; then
        text="<fc=$white,$red>$text</fc>"
    fi
fi

#echo "$text status=$status, now=$now, full=$full, wat=$wat, left=$left, time=$time, $time_text"
echo "$text"
