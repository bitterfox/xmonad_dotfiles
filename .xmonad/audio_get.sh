#!/bin/sh

isMute=`pactl list sinks | grep '^[[:space:]]Mute:' | head -n 1 | sed -r "s/.*: (.*)/\1/"`
volume=`pactl list sinks | grep '^[[:space:]]Volume:' | head -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,'`

if [ "$isMute" = "yes" ]; then
    echo "🔇$volume"
else
    echo "🔊$volume"
fi
