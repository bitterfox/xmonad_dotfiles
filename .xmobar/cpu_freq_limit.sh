#!/bin/bash
. $(dirname $0)/color.sh

minfreq=`$(dirname $0)/../.xmonad/system_scripts/pstate/get_min_freq.sh`
minfreq=`echo "scale=1\n($minfreq + 50000)/1000000" | bc`

maxfreq=`$(dirname $0)/../.xmonad/system_scripts/pstate/get_max_freq.sh`
maxfreq=`echo "scale=1\n($maxfreq + 50000)/1000000" | bc`

xmobar_printf "%1.1fGHz〜%1.1fGHz" $minfreq $maxfreq
