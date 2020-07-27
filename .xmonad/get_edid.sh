#!/bin/sh

x=$1
y=$2
width=$3
height=$4

rect="${width}x${height}+${x}+${y}"

echo "test"

#xrandr --verbose | grep -A1000 " connected $rect" | grep -A1 EDID | head -n 2 | tail -n 1 | awk '{print $1}'
