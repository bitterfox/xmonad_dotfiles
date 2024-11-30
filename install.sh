#!/bin/bash

PACKAGER=$1
if [ -z "PACKAGER" ]; then
    PACKAGER="apt"
fi
shift
PACKAGER_COMMAND="$@"
if [ -z "PACKAGER_COMMAND" ]; then
    PACKAGER_COMMAND="install"
fi

PACKAGES="xmonad xmobar ghc cabal-install libghc-parsec3-dev libghc-split-dev libghc-clock-dev dmenu gmrun trayer gnome-control-center gnome-settings-daemon network-manager-gnome libghc-xmonad-dev libghc-xmonad-contrib-dev ginn gnome-screensaver ttf-ancient-fonts-symbola dunst xdotool"

sudo $PACKAGER $PACKAGER_COMMAND $PACKAGES

cabal update
cabal install --lib xmonad xmonad-contrib extensible-exceptions clock X11 mtl split parsec utf8-string
