#!/bin/bash

start() {
    script=$1
    interval=$2
    output=$3
    size=${4:-1024}

    if [ -f "${output}.shmid" ]; then
        id="`cat ${output}.shmid`"
    else
        id=`/home/jp21734/git-repos/github.com/bitterfox/ssmcli/ssmcli_new $size`
        echo "$id" > ${output}.shmid
    fi

    while :; do
        # echo Update $script $interval $output
        start=`date +%s%N`
        o=`$(dirname $0)/$script`
        `/home/jp21734/git-repos/github.com/bitterfox/ssmcli/ssmcli_set $id "$o"`
        end=`date +%s%N`
        s="`echo "scale=1; $interval - ($end-$start)/1000./1000/1000" | bc`"
        sleep $s
    done
}

start xmobar.sh 1.5 /tmp/xmobar_metrics &


while :; do
    sleep 3600
done
