#!/bin/sh

index=`pacmd list-sinks | grep index | grep '\*' | sed -r "s/.*[^0-9]([0-9]+)/\1/"`

pactl set-sink-volume $index -5%
