. ~/.xmonad/color.sh

last_info=`cat /tmp/cpu_util_last`
cur_info=`cat /proc/stat | head -n 1`
echo $cur_info > /tmp/cpu_util_last

if [ -z "$last_info" ]; then
    percent="0"
else
    last_active=`echo $last_info | awk '{print $2 + $3 + $4}'`
    cur_active=`echo $cur_info | awk '{print $2 + $3 + $4}'`

    last_sum=`echo $last_info | awk 'a=0;{for(i=2;i<=NF;i++){a+=$i};print a}'`
    cur_sum=`echo $cur_info | awk 'a=0;{for(i=2;i<=NF;i++){a+=$i};print a}'`
#    echo $last_active $last_idle $cur_active $cur_idle

    percent=`echo "100 * ($cur_active - $last_active) / ($cur_sum - $last_sum)" | bc`
fi

text=`printf "❖%3d%%" $percent`
if [ $percent -ge 90 ]; then
    echo "<fc=$white,$red>$text</fc>"
else
    echo "$text"
fi
