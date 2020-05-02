. ~/.xmonad/color.sh

mem=$1

stat=`free -m | grep $1`
total=`echo $stat | awk '{print $2}'`
avail=`echo $stat | awk '{print $7}'`

if [ -z $avail ]; then
    used=`echo $stat | awk '{print $3}'`
else
    used=`echo "$total-$avail" | bc`
fi

ratio=`echo "scale=0\n100*$used/$total" | bc`

text=`printf "$1 %5dMB(%2d%%)" $used $ratio`
if [ $ratio -ge 90 ]; then
    echo "<fc=$white,$red>$text</fc>"
else
    echo "$text"
fi
