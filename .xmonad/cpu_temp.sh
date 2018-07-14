cat /sys/devices/platform/applesmc.768/temp9_input | xargs -i% echo -e 'scale=1\n%/1000' | bc
