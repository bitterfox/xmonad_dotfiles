#/bin/sh

#id=`xdotool search --class Dunst`
#echo $id

while :; do
    #    xdotool windowraise $id
    id=`xdotool search --class Dunst`
    if [ "$id" ]; then
        xargs xdotool windowraise $id
    fi
    sleep 1
done
