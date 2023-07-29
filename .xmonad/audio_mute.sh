#!/bin/sh

info=`pacmd list-sinks | grep -e index -e muted | grep -A1 '\*'`
index=`echo "$info" | grep index | sed -r "s/.*[^0-9]([0-9]+)/\1/"`
isMute=`echo "$info" | grep muted | awk '{print $2}'`

if [ "$isMute" = "yes" ]; then
    pacmd set-sink-mute $index 0
else
    pacmd set-sink-mute $index 1
fi

killall -SIGUSR1 xmobar_metrics_daemon
