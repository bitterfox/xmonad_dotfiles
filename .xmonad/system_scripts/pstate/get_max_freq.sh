#!/bin/bash

perf_pct=`$(dirname $0)/get_max.sh`

min_freq="`cat /sys/devices/system/cpu/cpufreq/policy0/cpuinfo_min_freq`"
max_freq="`cat /sys/devices/system/cpu/cpufreq/policy0/cpuinfo_max_freq`"

freq=$((max_freq / 100 * perf_pct))

if [ $freq -lt $min_freq ]; then
    freq=$min_freq
fi

if [ $freq -gt $max_freq ]; then
    freq=$max_freq
fi

freq=$(((freq + 99999)/100000*100000))

echo $freq
