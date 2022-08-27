#!/bin/bash
export DISPLAY=:0

/usr/bin/xhost +SI:localuser:root

basedir=$(dirname $0)

$basedir/set.sh `$basedir/get_current.sh`

while :; do
    # Wait monitor off
    echo "Wait monitor off"
    while :; do
        stat=`xset q | grep Monitor`
        echo $stat
        # Monitor is On/Off
        if [[ "$stat" == *"Monitor is On"* ]]; then
            echo "sleep 3"
            sleep 3
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
            echo "sleep 1"
            sleep 1
        fi
    done

    # Reset monitor brightness
    echo "Reset monitor brightness: `$basedir/get_current.sh`"
    $basedir/set.sh `$basedir/get_current.sh`
done
