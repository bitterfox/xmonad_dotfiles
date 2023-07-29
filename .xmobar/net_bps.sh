#!/bin/bash

. $(dirname $0)/color.sh

shmid_output="/tmp/xmobar_net_bps_util_last.shmid"

if [ -f "$shmid_output" ]; then
    id=`cat $shmid_output`
else
    id=`/home/jp21734/git-repos/github.com/bitterfox/ssmcli/ssmcli_new 1024`
    /home/jp21734/git-repos/github.com/bitterfox/ssmcli/ssmcli_set $id "0 0 0"
    echo "$id" > $shmid_output
fi

nic="enx00e04c0a135f"

last_info=`/home/jp21734/git-repos/github.com/bitterfox/ssmcli/ssmcli_get $id`

cur_rx_bytes=`cat /sys/class/net/$nic/statistics/rx_bytes 2> /dev/null`
if [ -z "$cur_rx_bytes" ]; then
    cur_rx_bytes="0"
fi

cur_tx_bytes=`cat /sys/class/net/$nic/statistics/tx_bytes 2> /dev/null`
if [ -z "$cur_tx_bytes" ]; then
    cur_tx_bytes="0"
fi

cur_millis=`echo $(($(date +%s%N)/1000000))`
/home/jp21734/git-repos/github.com/bitterfox/ssmcli/ssmcli_set $id "$cur_rx_bytes $cur_tx_bytes $cur_millis"

if [ -z "$last_info" ]; then
    rx_bps=0
    tx_bps=0
else
    last_rx_bytes=`echo $last_info | awk '{print $1}'`
    last_tx_bytes=`echo $last_info | awk '{print $2}'`
    last_millis=`echo $last_info | awk '{print $3}'`

    rx_bytes=`echo "$cur_rx_bytes - $last_rx_bytes" | bc`
    tx_bytes=`echo "$cur_tx_bytes - $last_tx_bytes" | bc`
    millis=`echo "$cur_millis - $last_millis" | bc`
    rx_bps=`echo "$rx_bytes * 1000 / $millis * 8" | bc`
    tx_bps=`echo "$tx_bytes * 1000 / $millis * 8" | bc`
fi

rx_prefix=""
rx_suffix=""
if [ $rx_bps -gt 1048576 ]; then
    rx_prefix="<fc=$white,$red>"
    rx_suffix="</fc>"
fi

tx_prefix=""
tx_suffix=""
if [ $tx_bps -gt 1048576 ]; then
    tx_prefix="<fc=$white,$red>"
    tx_suffix="</fc>"
fi


rx_unit="bps"
if [ $rx_bps -gt 1024 ]; then
    rx_bps=`echo "$rx_bps / 1024" | bc`
    rx_unit="Kbps"
fi
if [ $rx_bps -gt 1024 ]; then
    rx_bps=`echo "$rx_bps / 1024" | bc`
    rx_unit="Mbps"
fi

tx_unit="bps"
if [ $tx_bps -gt 1024 ]; then
    tx_bps=`echo "$tx_bps / 1024" | bc`
    tx_unit="Kbps"
fi
if [ $tx_bps -gt 1024 ]; then
    tx_bps=`echo "$tx_bps / 1024" | bc`
    tx_unit="Mbps"
fi

rx_text=`printf "⬇%4d%4s" $rx_bps $rx_unit`
tx_text=`printf "⬆️%4d%4s" $tx_bps $tx_unit`

xmobar_echo "📶$rx_prefix$rx_text$rx_suffix$tx_prefix$tx_text$tx_suffix"
