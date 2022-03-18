#!/bin/bash

freq=$1

min_freq="`cat /sys/devices/system/cpu/cpufreq/policy0/cpuinfo_min_freq`"
max_freq="`cat /sys/devices/system/cpu/cpufreq/policy0/cpuinfo_max_freq`"

if [ $freq -lt $min_freq ]; then
    freq=$min_freq
fi
if [ $freq -gt $max_freq ]; then
    freq=$max_freq
fi

cur_max_freq=`$(dirname $0)/get_max_freq.sh`
if [ $freq -gt $cur_max_freq ]; then
    freq=$cur_max_freq
fi

pct=$((freq*100/max_freq))

sudo sh -c "echo '$pct' > /sys/devices/system/cpu/intel_pstate/min_perf_pct"