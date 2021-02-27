#!/bin/sh

info=`pacmd list-sinks | grep -e index -e "^\s*volume:" -e muted -e name: | grep -A3 '\*'`

name=`echo "$info" | grep name: | sed -r "s/.*<([^>]+)>/\1/"`
isMute=`echo "$info" | grep muted | awk '{print $2}'`
volume_left=`echo "$info" | grep volume | sed -r "s/.*(left[^,]+,).*/\1/" | sed -r "s/.*[^0-9]([0-9]+%).*/\1/"`
volume_right=`echo "$info" | grep volume | sed -r "s/.*(right.*)/\1/" | sed -r "s/.*[^0-9]([0-9]+%).*/\1/"`

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
    echo "🔇$volume_text($name)"
else
    echo "🔊$volume_text($name)"
fi
