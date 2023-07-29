#!/bin/bash
. $(dirname $0)/color.sh

bat="BAT0"
bat_info=`cat /sys/class/power_supply/$bat/charge_now /sys/class/power_supply/$bat/charge_full /sys/class/power_supply/$bat/status /sys/class/power_supply/$bat/current_now | xargs`
set -- $bat_info
now=$1
full=$2
status=$3
wat=$4

left="$((1000 * $now / $full))"
if [[ "$left" -gt 1000 ]]; then
    left="1000"
fi
left_1=$((left/10))
left_2=$((left-left_1*10))

if [ "$status" = "Charging" ]; then
    time="$(((full - now) * 100 / wat))"
elif [ "$status" = "Discharging" ]; then
    #time=`echo "scale=2; $now / $wat" | bc | xargs printf "%1.2f"`
    time="$((now * 100 / wat))"
else
    time="0"
fi
if [ "$time" = "0" ]; then
    time_hour=0
    time_min=0
else
    time_hour=$((time/100))
    time_min=$(((time - time_hour*100) * 60 / 100))
fi

text=`printf "🔋%3d.%d%%(%02d:%02d)" $left_1 $left_2 $time_hour $time_min`

if [ "$status" = "Charging" ]; then
    if [[ "$left_1" -ge 95 ]]; then
        ok
    fi
elif [ "$status" = "Full" ]; then
    ok
else
    if [[ "$left_1" -le 15 ]]; then
        emergency
    fi
fi

#echo "$text status=$status, now=$now, full=$full, wat=$wat, left=$left, time=$time, $time_text"
xmobar_echo "$text"
