#!/bin/sh

info=`pacmd list-sinks | grep -e index -e "^\s*volume:" -e muted | grep -A2 '\*'`

isMute=`echo "$info" | grep muted | awk '{print $2}'`
volume_left=`echo "$info" | grep volume | sed -r "s/.*(left[^,]+,).*/\1/" | sed -r "s/.*[^0-9]([0-9]+%).*/\1/"`
volume_right=`echo "$info" | grep volume | sed -r "s/.*(right.*)/\1/" | sed -r "s/.*[^0-9]([0-9]+%).*/\1/"`

if [ "$volume_left" = "$volume_right" ]; then
    volume_text="$volume_left"
else
    volume_text="$volume_left|$volume_right"
fi

if [ "$isMute" = "yes" ]; then
    echo "🔇$volume_text"
else
    echo "🔊$volume_text"
fi
