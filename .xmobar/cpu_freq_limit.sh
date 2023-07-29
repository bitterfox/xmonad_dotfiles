#!/bin/sh
. $(dirname $0)/color.sh

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
