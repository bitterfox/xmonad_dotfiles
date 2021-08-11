#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

CM_DIR=$HOME clipmenu_fzf --preview-window="down:30%"
