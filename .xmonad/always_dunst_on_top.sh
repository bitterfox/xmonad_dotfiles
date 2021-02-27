#/bin/sh

#id=`xdotool search --class Dunst`
#echo $id

while :; do
    #    xdotool windowraise $id
    if [ -n "`pgrep dmenu`" ]; then
	sleep 1
	continue
    fi
    
    id=`xdotool search --class Dunst`
    if [ "$id" ]; then
        xargs xdotool windowraise $id
    fi
    sleep 1
done
