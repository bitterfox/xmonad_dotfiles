#!/bin/bash

basedir=$(dirname $0)
. $basedir/xmobar_metrics_functions.sh

start=`date +%s%N`

wip_task=`wip_task`

export NIC="enp130s0"
system_metrics=`$basedir/main`
fan_speed=`fan_speed`

battery=`battery`
keyboard_battery=`keyboard_battery`
mouse_battery=`mouse_battery`

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

if [ -n "$wip_task" ]; then
    echo -n "$wip_task | "
fi

if [ -n "$battery" ]; then
    echo -n "$battery | "
fi

echo -n "$system_metrics | "

if [ -n "$fan_speed" ]; then
    echo -n "$fan_speed | "
fi

if [ -n "$brightness" ]; then
    echo -n "$brightness | "
fi


echo "$volume | $jadate (`printf "%4d" $(((end-start)/1000/1000))` ms)"
#echo "(`printf "%4d" $(((end-start)/1000/1000))` ms)"
