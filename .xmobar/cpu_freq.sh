#!/bin/bash
. $(dirname $0)/color.sh

freq=`lscpu | grep "CPU MHz" | awk '{print $3}'`
freq=`printf "scale=1\n($freq + 50)/1000\n" | bc`

xmobar_printf "∿%1.1fGHz" $freq
