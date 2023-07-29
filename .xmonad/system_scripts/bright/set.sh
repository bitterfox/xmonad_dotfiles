#!/bin/bash

new=$1

current="`cat "/sys/class/backlight/acpi_video0/brightness"`"
max="`cat "/sys/class/backlight/acpi_video0/max_brightness"`"

if [ $new -lt 1 ]; then
    new=1
fi

if [ $new -gt $max ]; then
    new=$max
fi

pkexec /usr/libexec/gsd-backlight-helper /sys/class/backlight/acpi_video0 $new

killall -SIGUSR1 xmobar_metrics_daemon
