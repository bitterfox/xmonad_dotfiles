#!/bin/bash

basedir=$(dirname $0)
. $basedir/xmobar_metrics_functions.sh

#t=""
start=`date +%s%N`
#end=`date +%s%N` ; t="$t $(((end - start)/1000/1000))" ; start=`date +%s%N`

wip_task=`wip_task`
battery=`battery`
keyboard_battery=`keyboard_battery`
mouse_battery=`mouse_battery`
cpu_temp=`cpu_temp`
fan_speed=`fan_speed`
cpu_util=`cpu_util`
cpu_freq=`cpu_freq`
cpu_freq_limit=`cpu_freq_limit`
mem=`memory Mem 🍫`
swap=`memory Swap 🔃`
net=`net_bps`
net_segment_retransmit=`net_segment_retransmit`
brightness=`brightness`
volume=`volume`
jadate=`date "+%_m/%_d(%a) %H:%M"`

end=`date +%s%N`

if [ -n "$keyboard_battery" ]; then
    battery="$battery $keyboard_battery"
fi
if [ -n "$mouse_battery" ]; then
    battery="$battery $mouse_battery"
fi

echo "$wip_task | ${battery} | $cpu_temp$fan_speed | $cpu_util $cpu_freq($cpu_freq_limit) | $mem $swap | $net ($net_segment_retransmit) | ☀$brightness | $volume | $jadate (`printf "%4d" $(((end-start)/1000/1000))` ms)"
