#!/bin/bash

basedir=`dirname $0`
path="/tmp/xmonad_game_mode"
status=`cat $path`

echo $status

if [[ "$status" == "on" ]]; then
    echo disable game
    # $basedir/auto_detect_display.sh
    # xrandr --output HDMI-0 --mode 3840x2160 --scale 1.175x1.175 --rate 120  --primary --pos 0x0
    xset -dpms
    echo off > $path
else
    echo enable game
    #xrandr --output HDMI-0 --mode 3840x2160 --scale 1x1 --rate 240 --primary --pos 0x0 #--output DP-2 --mode 3840x2160 --scale 1x1 --pos 5760x0 --output DP-0 --mode 3840x2160 --rotate right --pos 0x4080 --output DP-1 --off --output DP-3 --off --output DP-4 --off --output DP-5 --off
    xset +dpms
    echo on > $path
fi
