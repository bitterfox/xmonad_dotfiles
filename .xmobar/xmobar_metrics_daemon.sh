#!/bin/bash

trap 'echo "Sleep interrupted"' SIGUSR1

start() {
    script=$1
    interval=$2
    output=$3
    size=${4:-1024}

    while :; do
        # echo Update $script $interval $output
        start=`date +%s%N`
        $(dirname $0)/$script > /tmp/xmobar_metrics.tmp
        mv /tmp/xmobar_metrics.tmp /tmp/xmobar_metrics

        end=`date +%s%N`
        s="`echo "scale=1; $interval - ($end-$start)/1000./1000/1000" | bc`"
        sleep $s &
        wait $!
    done
}

start xmobar.sh 2 /tmp/xmobar_metrics
