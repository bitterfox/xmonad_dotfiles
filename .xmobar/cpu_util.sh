. $(dirname $0)/color.sh

last_info=`cat /tmp/xmobar_cpu_util_last.$PPID`
cur_info=`cat /proc/stat | head -n 1`
echo $cur_info > /tmp/xmobar_cpu_util_last.$PPID

if [ -z "$last_info" ]; then
    percent="0"
else
    last_active=`echo $last_info | awk '{print $2 + $3 + $4}'`
    cur_active=`echo $cur_info | awk '{print $2 + $3 + $4}'`

    last_sum=`echo $last_info | awk 'BEGIN { ORS="" };{for(i=2;i<=NF;i++){print $i"+"}}; {print "0\n"}' | bc `
    cur_sum=`echo $cur_info | awk 'BEGIN { ORS="" };{for(i=2;i<=NF;i++){print $i"+"}}; {print "0\n"}' | bc `

    percent=`echo "100 * ($cur_active - $last_active) / ($cur_sum - $last_sum)" | bc`
fi

text=`printf "❖%3d%%" $percent`
if [ $percent -ge 90 ]; then
    emergency
fi
xmobar_echo "$text"
