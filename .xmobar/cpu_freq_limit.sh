#!/bin/sh
. $(dirname $0)/color.sh

minfreq=`intel-pstate-utils-get-freq min`
minfreq=`echo "scale=1\n($minfreq + 50000)/1000000" | bc`

maxfreq=`intel-pstate-utils-get-freq max`
maxfreq=`echo "scale=1\n($maxfreq + 50000)/1000000" | bc`

xmobar_printf "%1.1fGHz〜%1.1fGHz" $minfreq $maxfreq
