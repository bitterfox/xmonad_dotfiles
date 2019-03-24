#!/bin/sh

active_sink_id=`pactl list sinks | grep -B1 RUNNING | head -n 1 | sed -r "s/.*#([0-9]+)/\1/"`

pactl set-sink-volume $active_sink_id +5%
