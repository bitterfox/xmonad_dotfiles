# Add follow to sudoers
# bitterfox ALL=(ALL:ALL) NOPASSWD: /usr/bin/tee /sys/class/leds/spi\:\:kbd_backlight/brightness

current=`cat /sys/class/leds/smc::kbd_backlight/brightness`
max=`cat /sys/class/leds/smc::kbd_backlight/max_brightness`
new=$(( current - max / 10 ))
if [ $new -lt 1 ]; then
    new=0
fi
#echo $new | sudo tee /sys/class/leds/spi::kbd_backlight/brightness
echo $new | sudo tee /sys/class/leds/smc::kbd_backlight/brightness
