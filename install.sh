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

XMONAD_PACKAGES="ghc cabal-install xmonad libghc-parsec3-dev libghc-split-dev libghc-clock-dev libghc-xmonad-dev libghc-xmonad-contrib-dev"
XMONAD_TOOLS_PACKAGES="xmobar dunst dmenu gmrun trayer ginn xdotool"
GNOME_PACKAGES="gnome-control-center gnome-settings-daemon gnome-screenshot gnome-screensaver network-manager-gnome"
FONT_PACKAGES="ttf-ancient-fonts-symbola"
TERMINAL_ACTION_PACKAGES="sqlite3"
DESKTOP_UTIL_PACKAGES="compton"
OTHER_PACKAGES="feh solaar"
PACKAGES="$XMONAD_PACKAGES $XMONAD_TOOLS_PACKAGES $GNOME_PACKAGES $FONT_PACKAGES $TERMINAL_ACTION_PACKAGES $DESKTOP_UTIL_PACKAGES $OTHER_PACKAGES"

sudo $PACKAGER $PACKAGER_COMMAND $PACKAGES

cabal update
cabal install --lib xmonad xmonad-contrib extensible-exceptions clock X11 split utf8-string
# mtl parsec
