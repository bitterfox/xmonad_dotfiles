#!/bin/bash

while :; do
    echo Wait for X11 start up
    xset q
    if [ $? -eq 0 ]; then
        break;
    fi
    sleep 1
done
#xhost +SI:localuser:root; sleep 1

echo xset r rate 250 50
xset r rate 250 50
xset q

sleep 10

echo "Start xkeysnail"
/usr/local/bin/xkeysnail --watch -q $HOME/config.py
