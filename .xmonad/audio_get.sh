#!/bin/sh

active_sink_id=`pactl list sinks | grep -B1 RUNNING | head -n 1 | sed -r "s/.*#([0-9]+)/\1/"`

sinks=`pactl list sinks | sed -n '/Sink #'"$active_sink_id"'/,$p'`
isMute=`echo "$sinks" | grep '^[[:space:]]Mute:' | head -n 1 | sed -r "s/.*: (.*)/\1/"`
volume=`echo "$sinks" | grep '^[[:space:]]Volume:' | head -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,'`

if [ "$isMute" = "yes" ]; then
    echo "🔇$volume%($active_sink_id)"
else
    echo "🔊$volume%($active_sink_id)"
fi
