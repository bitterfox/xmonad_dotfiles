#!/bin/bash

start() {
    script=$1
    interval=$2
    output=$3
    while :; do
#        echo Update $script $interval $output
        $(dirname $0)/$script > $output.tmp
        mv $output.tmp $output
        sleep $interval
    done
}

start net_bps.sh 1 /tmp/xmobar_net_bps &
start cpu_freq.sh 2 /tmp/xmobar_cpu_freq &

while :; do
    sleep 3600
done
