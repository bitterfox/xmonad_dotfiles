#!/bin/bash
. $(dirname $0)/color.sh

freq=`lscpu | grep "CPU MHz" | awk '{print $3}'`
freq=`echo "scale=1\n($freq + 50)/1000" | bc`

minfreq=`$(dirname $0)/../.xmonad/system_scripts/pstate/get_min_freq.sh`
minfreq=`echo "scale=1\n($minfreq + 50000)/1000000" | bc`

maxfreq=`$(dirname $0)/../.xmonad/system_scripts/pstate/get_max_freq.sh`
maxfreq=`echo "scale=1\n($maxfreq + 50000)/1000000" | bc`

xmobar_printf "∿%1.1fGHz(%1.1fGHz〜%1.1fGHz)" $freq $minfreq $maxfreq
