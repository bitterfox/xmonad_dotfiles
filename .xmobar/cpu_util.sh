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
