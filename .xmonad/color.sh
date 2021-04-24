black="#4E4B42"
brightBlack="#635F54"
gray="#B4AF9A"
darkWhite="#CDC8B0"
white="#DAD4BB"
red="#CC654C"
blue="#3BA99F"
brightBlue="#42bdb7"
brightRed="#e06d56"

xmobar_echo() {
    echo "<fc=$fg,$bg>$@</fc>"
}

xmobar_printf() {
    echo -n "<fc=$fg,$bg>"
    printf $@
    echo -n "</fc>"
}

set_fg() {
    fg="$1"
}

set_bg() {
    bg="$1"
}

#. "`dirname $0`/theme_bright.sh"
. "`dirname $0`/theme_dark.sh"

reset
