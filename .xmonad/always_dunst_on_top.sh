#/bin/sh

#id=`xdotool search --class Dunst`
#echo $id

while :; do
    #    xdotool windowraise $id
    xdotool search --class Dunst | xargs xdotool windowraise
    sleep 0.3
done
