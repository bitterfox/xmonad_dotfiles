. ~/.xmonad/color.sh

last_info=`cat /tmp/xmobar_net_util_last.$PPID`
cur_rx_bytes=`cat /sys/class/net/wlp3s0/statistics/rx_bytes`
cur_tx_bytes=`cat /sys/class/net/wlp3s0/statistics/tx_bytes`
echo $cur_rx_bytes $cur_tx_bytes  > /tmp/xmobar_net_util_last.$PPID

if [ -z "$last_info" ]; then
    rx_bytes=0
    tx_bytes=0
else
    last_rx_bytes=`echo $last_info | awk '{print $1}'`
    last_tx_bytes=`echo $last_info | awk '{print $2}'`

    rx_bytes=`echo "$cur_rx_bytes - $last_rx_bytes" | bc`
    tx_bytes=`echo "$cur_tx_bytes - $last_tx_bytes" | bc`
fi

rx_prefix=""
rx_suffix=""
if [ $rx_bytes -gt 1048576 ]; then
    rx_prefix="<fc=$white,$red>"
    rx_suffix="</fc>"
fi

tx_prefix=""
tx_suffix=""
if [ $tx_bytes -gt 1048576 ]; then
    tx_prefix="<fc=$white,$red>"
    tx_suffix="</fc>"
fi


rx_unit="B"
if [ $rx_bytes -gt 1024 ]; then
    rx_bytes=`echo "$rx_bytes / 1024" | bc`
    rx_unit="KB"
fi

tx_unit="B"
if [ $tx_bytes -gt 1024 ]; then
    tx_bytes=`echo "$tx_bytes / 1024" | bc`
    tx_unit="KB"
fi

rx_text=`printf "⬇%4d%2s" $rx_bytes $rx_unit`
tx_text=`printf "⬆️%4d%2s" $tx_bytes $tx_unit`

echo -n "📶$rx_prefix$rx_text$rx_suffix|$tx_prefix$tx_text$tx_suffix"

echo "$text"
