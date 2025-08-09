#!/bin/bash

. $(dirname $0)/color.sh

battery() {
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
}

keyboard_battery() {
    keyboard="Keychron Q10 Max"
    battery=`upower --dump | grep -e model -e percentage | grep -A1 "$keyboard" | grep percentage | sed -r "s/.*percentage:[^0-9]*([0-9]+)%/\1/"`

    if [ -z "$battery" ]; then
        exit 0
    fi

    if [ $battery -le 20 ]; then
        emergency
    elif [ $battery -ge 80 ]; then
        ok
    fi

    xmobar_echo "🖮 ${battery}%"
}

mouse_battery() {
    current_timestamp=`date +%s`
    last_mouse_battery_result=`cat /tmp/xmobar_mouse_battery`
    if [ -z "$last_mouse_battery_result" ]; then
        echo $current_timestamp -1 > /tmp/xmobar_mouse_battery
        update_mouse_battery $current_timestamp &
        exit 0
    fi
    battery=`echo "$last_mouse_battery_result" | awk '{print $2}'`
    last_mouse_battery_timestamp=`echo "$last_mouse_battery_result" | awk '{print $1}'`
    if [ $(( last_mouse_battery_timestamp + 60 )) -le $current_timestamp ]; then
        echo $current_timestamp $battery > /tmp/xmobar_mouse_battery
        update_mouse_battery $current_timestamp &
    fi

    if [ $battery -le 0 ]; then
        exit 0
    fi

    if [ $battery -le 20 ]; then
        emergency
    elif [ $battery -ge 80 ]; then
        ok
    fi

    xmobar_echo "🖰 $battery%"
}

update_mouse_battery() {
    start=`date +%s%N`
    battery=`solaar show | grep Battery | head -n 1 | sed -r "s/.*Battery:[^0-9]*([0-9]+)%.*/\1/"`
    end=`date +%s%N`

    if [ -n "$battery" ]; then
        echo $1 $battery $(( end - start )) > /tmp/xmobar_mouse_battery
    else
        echo $1 -1 > /tmp/xmobar_mouse_battery
    fi
}

fan_speed() {
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
}

brightness() {
    current="`cat "/sys/class/backlight/acpi_video0/brightness"`"
    if [ -z "$current" ]; then
        return
    fi

    max="`cat "/sys/class/backlight/acpi_video0/max_brightness"`"
    text="`echo "scale=2;b=$current / $max*100;scale=0;b/1" | bc | xargs printf "%3d%%"`"

    xmobar_echo "☀$text"
}

volume() {
    info=`pacmd list-sinks | grep -e index -e "^\s*volume:" -e muted -e name: | grep -A3 '\*'`

    name=""
    isMute=""
    volume_left=""
    volume_right=""
    {
        read line # index
        read line # name
        name=`sed -r "s/.*<([^>]+)>/\1/" <<< "$line"`
        read line # volume
        volume_left=`awk -F, '{print $1}' <<< "$line" | awk -F/ '{print $2}'`
        volume_right=`awk -F, '{print $2}' <<< "$line" | awk -F/ '{print $2}'`
        read line # muted
        isMute=`awk '{print $2}' <<< "$line"`
    } <<< "$info"

    if [ "$volume_left" = "$volume_right" ]; then
        volume_text=`printf "%4s" $volume_left`
    else
        volume_text=`printf "%4s|%4s" $volume_left $ volume_right`
    fi

    case "$name" in
        "alsa_output.pci-0000_00_1f.3.hdmi-stereo-extra1" ) name="HDMI" ;;
        "alsa_output.pci-0000_00_1f.3.analog-stereo.equalizer" ) name="EQUA" ;;
        "alsa_output.pci-0000_00_1f.3.analog-stereo" ) name="HEAD" ;;
        "bluez_sink.94_DB_56_89_17_0A.a2dp_sink" ) name="SONY" ;;
        * ) name="UNKW" ;;
    esac

    if [ "$isMute" = "yes" ]; then
        xmobar_echo "🔇$volume_text($name)"
    else
        xmobar_echo "🔊$volume_text($name)"
    fi
}

wip_task() {
    if [ ! -f ~/git-repos/github.com/bitterfox/fzflet/jira/jira_worklog_preview_task.sh ]; then
        exit 0
    fi

    str=`~/git-repos/github.com/bitterfox/fzflet/jira/jira_worklog_preview_task.sh`
    if [ -z "$str" ]; then
        echo ""
        exit 0
    fi

    max_len=40
    str_len=`expr length "$str"`
    ticket=`echo "$str" | awk '{print $1}'`
    if [ $str_len -le $max_len ]; then
        echo "⏳$str"
        exit 0
    fi

    task=`echo "$str" | sed -r "s/(.*) \((.*)\)/\1/"`
    time=`echo "$str" | sed -r "s/(.*) \((.*)\)/... (\2)/"`

    time_len=`expr length "$time"`
    task=`echo "$str" | cut -c -$((max_len - time_len))`

    echo "⏳$task$time"
}
