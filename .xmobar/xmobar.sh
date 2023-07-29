#!/bin/bash

basedir=$(dirname $0)

start=`date +%s%N`

battery=`$basedir/battery.sh`
cpu_util=`$basedir/cpu_util.sh`
cpu_freq=`$basedir/cpu_freq.sh`
net=`$basedir/net_bps.sh`
wip_task=`$basedir/wip_task.sh`

cpu_freq_limit=`$basedir/cpu_freq_limit.sh`
cpu_temp=`$basedir/cpu_temp.sh`
fan_speed=`$basedir/fan_speed.sh`
mem=`$basedir/memory.sh Mem 🍫`
swap=`$basedir/memory.sh Mem 🔃`
net_segment_retransmit=`$basedir/net_segment_retransmit.sh`
brightness=`$basedir/bright_get.sh`
volume=`$basedir/audio_get.sh`
jadate=`date "+%_m/%_d(%a) %H:%M"`

end=`date +%s%N`


echo "$wip_task | $battery | $cpu_temp$fan_speed | $cpu_util $cpu_freq($cpu_freq_limit) | $mem $swap | $net ($net_segment_retransmit) | ☀$brightness | $volume | $jadate (`printf "%4d" $(((end-start)/1000/1000))` ms)"
