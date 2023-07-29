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

cpu_util() {
    last_info=`cat /tmp/xmobar_cpu_util_last`
    cur_info=`cat /proc/stat | head -n 1`
    echo $cur_info > /tmp/xmobar_cpu_util_last

    if [ -z "$last_info" ]; then
        percent="0"
    else
        set -- $last_info
        shift
        last_active="$(($1 + $2 + $3))"
        last_sum=0
        for i in $@; do
            last_sum="$((last_sum + $i))"
        done

        set -- $cur_info
        shift
        cur_active="$(($1 + $2 + $3))"
        cur_sum=0
        for i in $@; do
            cur_sum="$((cur_sum + $i))"
        done

        percent="$((100 * ($cur_active - $last_active) / ($cur_sum - $last_sum)))"
    fi

    text=`printf "❖%3d%%" $percent`
    if [ $percent -ge 90 ]; then
        emergency
    fi
    xmobar_echo "$text"
}

cpu_freq() {
    freq=`lscpu | grep "CPU MHz" | awk '{print $3}'`
    freq=`printf "scale=1\n($freq + 50)/1000\n" | bc`

    xmobar_printf "∿%1.1fGHz" $freq
}

cpu_freq_limit() {
    lower_bound_freq_pct="`cat /sys/devices/system/cpu/intel_pstate/min_perf_pct`"
    upper_bound_freq_pct="`cat /sys/devices/system/cpu/intel_pstate/max_perf_pct`"
    min_freq="`cat /sys/devices/system/cpu/cpufreq/policy0/cpuinfo_min_freq`"
    max_freq="`cat /sys/devices/system/cpu/cpufreq/policy0/cpuinfo_max_freq`"

    lower_bound_freq=$((max_freq / 100 * lower_bound_freq_pct))
    upper_abound_freq=$((max_freq / 100 * upper_bound_freq_pct))

    if [ $lower_bound_freq -lt $min_freq ]; then
        lower_bound_freq=$min_freq
    elif [ $lower_bound_freq -gt $max_freq ]; then
        lower_bound_freq=$max_freq
    fi

    if [ $upper_abound_freq -lt $min_freq ]; then
        upper_abound_freq=$min_freq
    elif [ $upper_abound_freq -gt $max_freq ]; then
        upper_abound_freq=$max_freq
    fi

    lower_bound_freq_gz="`echo "scale=1; ($lower_bound_freq + 99999) / 1000/ 1000" | bc`"
    upper_bound_freq_gz="`echo "scale=1; ($upper_abound_freq + 99999) / 1000/ 1000" | bc`"

    xmobar_printf "%1.1fGHz〜%1.1fGHz" $lower_bound_freq_gz $upper_bound_freq_gz
}

cpu_temp() {
    temp=`cat /sys/devices/platform/coretemp.0/hwmon/hwmon*/temp1_input | xargs -i% echo -e 'scale=1\n%/1000' | bc`

    throttle_count=`cat /sys/devices/system/cpu/cpu0/thermal_throttle/package_throttle_count`

    text="🌡$temp℃ ($throttle_count)"

    if [ `echo "80 <= $temp" | bc` = 1 ]; then
        emergency
    elif [ `echo "50 >= $temp" | bc` = 1 ]; then
        ok
    fi
    xmobar_echo $text
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

memory() {
    mem=$1
    label=$2

    stat=`free -m | grep $1`
    total=`echo $stat | awk '{print $2}'`
    #avail=`echo $stat | awk '{print $7}'`

    #if [ -z "$avail" ]; then
    used=`echo $stat | awk '{print $3}'`
    #else
    #    used=`echo "$total-$avail" | bc`
    #fi

    #ratio=`echo "scale=0;100*$used/$total" | bc`
    ratio="$((100*$used/$total))"

    text=`printf "$label %5dMB(%2d%%)" $used $ratio`
    if [[ $ratio -ge 90 ]]; then
        emergency
    fi
    xmobar_echo "$text"
}

net_bps() {
    nic="enx00e04c0a135f"

    last_info="0 0 0"
    if [ -f "/tmp/xmobar_net_bps_util_last" ]; then
        last_info=`cat /tmp/xmobar_net_bps_util_last`
    fi

    cur_rx_bytes=`cat /sys/class/net/$nic/statistics/rx_bytes 2> /dev/null`
    if [ -z "$cur_rx_bytes" ]; then
        cur_rx_bytes="0"
    fi

    cur_tx_bytes=`cat /sys/class/net/$nic/statistics/tx_bytes 2> /dev/null`
    if [ -z "$cur_tx_bytes" ]; then
        cur_tx_bytes="0"
    fi

    cur_millis=`echo $(($(date +%s%N)/1000000))`
    echo $cur_rx_bytes $cur_tx_bytes $cur_millis > /tmp/xmobar_net_bps_util_last

    if [ -z "$last_info" ]; then
        rx_bps=0
        tx_bps=0
    else
        set -- $last_info
        last_rx_bytes="$1"
        last_tx_bytes="$2"
        last_millis="$3"

        rx_bps="$((($cur_rx_bytes - $last_rx_bytes) * 1000 / ($cur_millis - $last_millis) * 8))"
        tx_bps="$((($cur_tx_bytes - $last_tx_bytes) * 1000 / ($cur_millis - $last_millis) * 8))"
    fi

    rx_prefix=""
    rx_suffix=""
    if [ $rx_bps -gt 1048576 ]; then
        rx_prefix="<fc=$white,$red>"
        rx_suffix="</fc>"
    fi

    tx_prefix=""
    tx_suffix=""
    if [ $tx_bps -gt 1048576 ]; then
        tx_prefix="<fc=$white,$red>"
        tx_suffix="</fc>"
    fi


    rx_unit="bps"
    if [ $rx_bps -gt 1048576 ]; then
        rx_bps="$(($rx_bps / 1048576))"
        rx_unit="Mbps"
    elif [ $rx_bps -gt 1024 ]; then
        rx_bps="$(($rx_bps / 1024))"
        rx_unit="Kbps"
    fi

    tx_unit="bps"
    if [ $tx_bps -gt 1048576 ]; then
        tx_bps="$(($tx_bps / 1048576))"
        tx_unit="Mbps"
    elif [ $tx_bps -gt 1024 ]; then
        tx_bps="$(($tx_bps / 1024))"
        tx_unit="Kbps"
    fi

    rx_text=`printf "⬇%4d%4s" $rx_bps $rx_unit`
    tx_text=`printf "⬆️%4d%4s" $tx_bps $tx_unit`

    xmobar_echo "📶$rx_prefix$rx_text$rx_suffix$tx_prefix$tx_text$tx_suffix"
}

net_segment_retransmit() {
    tcp_values=`cat /proc/net/snmp | grep "Tcp:" | head -n 2 | tail -n 1`
    set -- $tcp_values

    tcp_columns=`cat /proc/net/snmp | grep "Tcp:" | head -n 1`

    for column in $tcp_columns; do
        if [ "$column" == "RetransSegs" ]; then
            echo $1
            break
        fi
        shift
    done
}

brightness() {
    current="`cat "/sys/class/backlight/acpi_video0/brightness"`"
    max="`cat "/sys/class/backlight/acpi_video0/max_brightness"`"
    text="`echo "scale=2;b=$current / $max*100;scale=0;b/1" | bc | xargs printf "%3d%%"`"

    xmobar_echo "$text"
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
