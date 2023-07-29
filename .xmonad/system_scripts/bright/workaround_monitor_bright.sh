#!/bin/bash
export DISPLAY=:0

BITTER_FOX_WMB_SLEEP_ON="${BITTER_FOX_WMB_SLEEP_ON:-3}"
BITTER_FOX_WMB_SLEEP_OFF="${BITTER_FOX_WMB_SLEEP_ON:-1}"

/usr/bin/xhost +SI:localuser:root

basedir=$(dirname $0)

$basedir/set.sh `$basedir/get_current.sh`

while :; do
    # Wait monitor off
    if [ -n "$BITTER_FOX_WMB_DEBUG" ]; then
        echo "Wait monitor off"
    fi
    while :; do
        stat=`xset q | grep Monitor`
        if [ -n "$BITTER_FOX_WMB_DEBUG" ]; then
            echo $stat
        fi
        # Monitor is On/Off
        if [[ "$stat" == *"Monitor is On"* ]]; then
            if [ -n "$BITTER_FOX_WMB_DEBUG" ]; then
                echo "sleep $BITTER_FOX_WMB_SLEEP_ON"
            fi
            sleep $BITTER_FOX_WMB_SLEEP_ON
        else
            break
        fi
    done

    # Wait monitor on
    echo "Wait monitor on"
    while :; do
        stat=`xset q | grep Monitor`
        echo $stat
        # Monitor is On/Off
        if [[ "$stat" == *"Monitor is On"* ]]; then
            break
        else
            if [ -n "$BITTER_FOX_WMB_DEBUG" ]; then
                echo "sleep $BITTER_FOX_WMB_SLEEP_OFF"
            fi
            sleep $BITTER_FOX_WMB_SLEEP_OFF
        fi
    done

    # Reset monitor brightness
    echo "Reset monitor brightness: `$basedir/get_current.sh`"
    $basedir/set.sh `$basedir/get_current.sh`
done
