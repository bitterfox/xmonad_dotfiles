#!/bin/bash

. $(dirname $0)/color.sh

info=`pacmd list-sinks | grep -e index -e "^\s*volume:" -e muted -e name: | grep -A3 '\*'`

name=""
isMute=""
volume_left=""
volume_right=""
{
    read line # index
    read line # name
    name=`sed -r "s/.*<([^>]+)>/\1/" <<< "$line"`
    read line # volume
    volume_left=`awk -F, '{print $1}' <<< "$line" | awk -F/ '{print $2}'`
    volume_right=`awk -F, '{print $2}' <<< "$line" | awk -F/ '{print $2}'`
    read line # muted
    isMute=`awk '{print $2}' <<< "$line"`
} <<< "$info"

if [ "$volume_left" = "$volume_right" ]; then
    volume_text=`printf "%4s" $volume_left`
else
    volume_text=`printf "%4s|%4s" $volume_left $ volume_right`
fi

case "$name" in
    "alsa_output.pci-0000_00_1f.3.hdmi-stereo-extra1" ) name="HDMI" ;;
    "alsa_output.pci-0000_00_1f.3.analog-stereo.equalizer" ) name="EQUA" ;;
    "alsa_output.pci-0000_00_1f.3.analog-stereo" ) name="HEAD" ;;
    "bluez_sink.94_DB_56_89_17_0A.a2dp_sink" ) name="SONY" ;;
    * ) name="UNKW" ;;
esac

if [ "$isMute" = "yes" ]; then
    xmobar_echo "🔇$volume_text($name)"
else
    xmobar_echo "🔊$volume_text($name)"
fi
