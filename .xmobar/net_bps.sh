#!/bin/bash

. $(dirname $0)/color.sh

nic="enx00e04c0a135f"

last_info="0 0 0"
if [ -f "/tmp/xmobar_net_bps_util_last" ]; then
    last_info=`cat /tmp/xmobar_net_bps_util_last`
fi

cur_rx_bytes=`cat /sys/class/net/$nic/statistics/rx_bytes 2> /dev/null`
if [ -z "$cur_rx_bytes" ]; then
    cur_rx_bytes="0"
fi

cur_tx_bytes=`cat /sys/class/net/$nic/statistics/tx_bytes 2> /dev/null`
if [ -z "$cur_tx_bytes" ]; then
    cur_tx_bytes="0"
fi

cur_millis=`echo $(($(date +%s%N)/1000000))`
echo $cur_rx_bytes $cur_tx_bytes $cur_millis > /tmp/xmobar_net_bps_util_last

if [ -z "$last_info" ]; then
    rx_bps=0
    tx_bps=0
else
    set -- $last_info
    last_rx_bytes="$1"
    last_tx_bytes="$2"
    last_millis="$3"

    rx_bps="$((($cur_rx_bytes - $last_rx_bytes) * 1000 / ($cur_millis - $last_millis) * 8))"
    tx_bps="$((($cur_tx_bytes - $last_tx_bytes) * 1000 / ($cur_millis - $last_millis) * 8))"
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
if [ $rx_bps -gt 1048576 ]; then
    rx_bps="$(($rx_bps / 1048576))"
    rx_unit="Mbps"
elif [ $rx_bps -gt 1024 ]; then
    rx_bps="$(($rx_bps / 1024))"
    rx_unit="Kbps"
fi

tx_unit="bps"
if [ $tx_bps -gt 1048576 ]; then
    tx_bps="$(($tx_bps / 1048576))"
    tx_unit="Mbps"
elif [ $tx_bps -gt 1024 ]; then
    tx_bps="$(($tx_bps / 1024))"
    tx_unit="Kbps"
fi

rx_text=`printf "⬇%4d%4s" $rx_bps $rx_unit`
tx_text=`printf "⬆️%4d%4s" $tx_bps $tx_unit`

xmobar_echo "📶$rx_prefix$rx_text$rx_suffix$tx_prefix$tx_text$tx_suffix"
