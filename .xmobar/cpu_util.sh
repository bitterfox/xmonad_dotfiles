#!/bin/bash

. $(dirname $0)/color.sh

shmid_output="/tmp/xmobar_cpu_util_last.shmid"

if [ -f "$shmid_output" ]; then
    id=`cat $shmid_output`
else
    id=`/home/jp21734/git-repos/github.com/bitterfox/ssmcli/ssmcli_new 1024`
    echo "$id" > $shmid_output
fi

last_info=`/home/jp21734/git-repos/github.com/bitterfox/ssmcli/ssmcli_get $id`
cur_info=`cat /proc/stat | head -n 1`
/home/jp21734/git-repos/github.com/bitterfox/ssmcli/ssmcli_set $id "$cur_info"

if [ -z "$last_info" ]; then
    percent="0"
else
    set -- $last_info
    shift
    last_active="$(($1 + $2 + $3))"
    last_sum=0
    for i in $@; do
        last_sum="$((last_sum + $i))"
    done

    set -- $cur_info
    shift
    cur_active="$(($1 + $2 + $3))"
    cur_sum=0
    for i in $@; do
        cur_sum="$((cur_sum + $i))"
    done

    percent="$((100 * ($cur_active - $last_active) / ($cur_sum - $last_sum)))"
fi

text=`printf "❖%3d%%" $percent`
if [ $percent -ge 90 ]; then
    emergency
fi
xmobar_echo "$text"
