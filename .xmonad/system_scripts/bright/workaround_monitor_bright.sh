#!/bin/bash
export DISPLAY=:0

BITTER_FOX_WMB_SLEEP_ON="${BITTER_FOX_WMB_SLEEP_ON:-3}"
BITTER_FOX_WMB_SLEEP_OFF="${BITTER_FOX_WMB_SLEEP_ON:-1}"
BITTER_FOX_WMB_REPEAT_DELAY="${BITTER_FOX_WMB_REPEAT_DELAY:-210}"
BITTER_FOX_WMB_REPEAT_RATE="${BITTER_FOX_WMB_REPEAT_RATE:-70}"

/usr/bin/xhost +SI:localuser:root

basedir=$(dirname $0)

$basedir/set.sh `$basedir/get_current.sh`

while :; do
    # Wait monitor off
    if [ -n "$BITTER_FOX_WMB_DEBUG" ]; then
        echo "Wait monitor off"
    fi
    while :; do
        xset_q=`xset q`
        stat=`echo "$xset_q" | grep Monitor`
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

        auto_repeat_conf=`echo "$xset_q" | grep "auto repeat delay"`
        auto_repeat_delay=`echo "$auto_repeat_conf" | awk '{print $4}'`
        auto_repeat_rate=`echo "$auto_repeat_conf" | awk '{print $7}'`
        if [ -n "$BITTER_FOX_WMB_DEBUG" ]; then
            echo "auto repeat delay $auto_repeat_delay auto repeat rate $auto_repeat_rate"
        fi
        if [ "$auto_repeat_delay" -ne $BITTER_FOX_WMB_REPEAT_DELAY ] || [ "$auto_repeat_rate" -ne $BITTER_FOX_WMB_REPEAT_RATE ]; then
            xset r rate $BITTER_FOX_WMB_REPEAT_DELAY $BITTER_FOX_WMB_REPEAT_RATE
        fi
    done

    # Wait monitor on
    if [ -n "$BITTER_FOX_WMB_DEBUG" ]; then
        echo "Wait monitor on"
    fi
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
    if [ -n "$BITTER_FOX_WMB_DEBUG" ]; then
        echo "Reset monitor brightness: `$basedir/get_current.sh`"
    fi
    $basedir/set.sh `$basedir/get_current.sh`
done
