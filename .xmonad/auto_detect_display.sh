#!/bin/bash

path="/tmp/xmonad_game_mode"
status=`cat $path`

echo $status

if [[ "$status" == "on" ]]; then
    echo enabled gamemode
    xrandr --output HDMI-0 --mode 3840x2160 --scale 1x1 --rate 240 --primary --pos 0x0 --output DP-2 --mode 3840x2160 --scale 1x1 --pos 5760x0 --output DP-0 --mode 3840x2160 --rotate right --pos 0x4080 --output DP-1 --off --output DP-3 --off --output DP-4 --off --output DP-5 --off
else
    echo disabled game
    xrandr --output HDMI-0 --mode 3840x2160 --scale 1.175x1.175 --rate 120  --primary --pos 0x0 --output DP-2 --mode 3840x2160 --scale 1x1 --pos 5760x0 --output DP-0 --mode 3840x2160 --rotate right --pos 0x4080 --output DP-1 --off --output DP-3 --off --output DP-4 --off --output DP-5 --off
fi


#echo "xrandr --output HDMI-0 --mode 3840x2160 --scale 0.6x0.6 --primary --pos 0x0 --output DP-0 --mode 3840x2160 --rotate right --pos 0x4080 --output DP-1 --off --output DP-2 --mode 3840x2160 --pos 5760x0 --output DP-3 --off --output DP-4 --off --output DP-5 --off"


#xrandr --output HDMI-0 --mode 3840x2160 --scale 1x1 --rate 60 --primary --pos 0x0 --output DP-0 --mode 3840x2160 --rotate right --pos 0x4080 --output DP-1 --off --output DP-2 --mode 3840x2160 --scale 1x1 --pos 5760x0 --output DP-3 --off --output DP-4 --off --output DP-5 --off

#xrandr --output HDMI-0 --mode 3840x2160 --scale 1x1 --rate 240  --primary --pos 0x0 --output DP-0 --mode 3840x2160 --rotate right --pos 0x4080 --output DP-1 --off --output DP-2 --mode 3840x2160 --scale 0.8x0.8 --pos 5760x0 --output DP-3 --off --output DP-4 --off --output DP-5 --off

xrandr

exit 0

right_EDID="00ffffffffffff0010acb3414c333232"
scale_EDID="00ffffffffffff0010ac49a2534e4a30"
scale="--scale 1.17x1.17"

result=`xrandr`
result_verbose=`xrandr --verbose`

echo "xrandr result ---"
echo "$result"
echo "xrandr result ---"

num_connection=`echo "$result" | grep -c "connected"`

echo "$num_connection connection(s) found"

primary_width=0
primary_height=0

xrandr_param=""
num_connected_display=0


for i in `seq 1 $num_connection`; do
    echo $xrandr_param
    echo "$i========================================"
    connection_record=`echo "$result" | grep connected | head -n $i | tail -n 1`
    echo $connection_record

    connector=`echo "$connection_record" | awk '{print $1}'`
    echo $connector
    xrandr_param="$xrandr_param --output $connector"

    if [ "`echo \"$connection_record\
" | grep disconnected`" ]; then
        xrandr_param="$xrandr_param --off"
        continue
    fi
    num_connected_display=$((num_connected_display + 1))

    mode_record=`echo "$result" | grep -A1 "^$connector" | head -n 2 | tail -n 1`
    echo $mode_record

    mode=`echo "$mode_record" | awk '{print $1}'`
    echo "$i ($connector) Mode: $mode"
    xrandr_param="$xrandr_param --mode $mode"

    width=`echo "$result_verbose" | grep -A 9999 "$connector" | grep -A3 "$mode" | grep width | head -n 1 | sed -r "s/.*width[^0-9]+([0-9]+).*/\1/"`
    height=`echo "$result_verbose" | grep -A 9999 "$connector" | grep -A3 "$mode" | grep height | head -n 1 | sed -r "s/.*height[^0-9]+([0-9]+).*/\1/"`

    echo "$result_verbose" | grep -A 9999 "^$connector" | grep -A1 "EDID" | head -n 2 | tail -n 1
    next_connection_record=`echo "$result" | grep connected | head -n $((i+1)) | tail -n 1`
    next_connector=`echo "$next_connection_record" | awk '{print $1}'`
    echo "Next connector: $next_connector"
    EDID=`echo "$result_verbose" | grep -A 9999 "^$connector" | grep -B 9999 "^$next_connector" | grep -A1 "EDID" | head -n 2 | tail -n 1 | awk '{print $1}'`
    echo $EDID
    rotate=""
    if [ "`echo $right_EDID | grep "^$EDID$"`" ]; then
        echo right
        xrandr_param="$xrandr_param --rotate right"
        tmp=$width
        width=$height
        height=$tmp
    fi
    if [ "`echo $scale_EDID | grep "^$EDID$"`" ]; then
        echo scale
        xrandr_param="$xrandr_param $scale"
    fi

    echo $width $height

    if [ $num_connected_display -eq 1 ]; then
        # primary
        echo "primary display found"
        primary_width=$width
        primary_height=$height
        xrandr_param="$xrandr_param --primary"
    elif [ $num_connected_display -eq 2 ]; then
        echo "secondary display found"
        pos_y=$primary_height
        if [ $height -lt $primary_height ]; then
            pos_y=$((pos_y + primary_height / 2))
        else
            pos_y=$((pos_y + height / 2))
        fi
        xrandr_param="$xrandr_param --pos 0x$pos_y"
    elif [ $num_connected_display -eq 3 ]; then
        echo "third display found"
        pos_x=$primary_width
        if [ $width -lt $primary_width ]; then
            pos_x=$((pos_x + primary_width / 2))
        else
            pos_x=$((pos_x + width / 2))
        fi
        xrandr_param="$xrandr_param --pos ${pos_x}x0"
    elif [ $num_connected_display -eq 4 ]; then
        echo "fourth display found"
        pos_x=$primary_width
        if [ $width -lt $primary_width ]; then
            pos_x=$((pos_x + primary_width / 2))
        else
            pos_x=$((pos_x + width / 2))
        fi
        pos_y=$primary_height
        if [ $height -lt $primary_height ]; then
            pos_y=$((pos_y + primary_height / 2))
        else
            pos_y=$((pos_y + height / 2))
        fi
        xrandr_param="$xrandr_param --pos ${pos_x}x${pos_y}"
    fi
done
echo $xrandr_param

xrandr $xrandr_param

#00 ff ff ff ff ff ff 00
#10 ac
#b5 41
#4c 32
#33
#32
