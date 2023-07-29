#!/bin/zsh

basedir=`dirname $0`

time $basedir/bench.sh ./audio_get.sh
time $basedir/bench.sh ./battery.sh
time $basedir/bench.sh ./bright_get.sh
time $basedir/bench.sh ./cpu_freq.sh
time $basedir/bench.sh ./cpu_freq_limit.sh
time $basedir/bench.sh ./cpu_temp.sh
time $basedir/bench.sh ./cpu_util.sh
time $basedir/bench.sh ./fan_speed.sh
time $basedir/bench.sh ./memory.sh Mem M
time $basedir/bench.sh ./memory.sh Swap S
time $basedir/bench.sh ./net_bps.sh
time $basedir/bench.sh ./net_segment_retransmit.sh
time $basedir/bench.sh ./wip_task.sh
