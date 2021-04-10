#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

dmenu_path | fzf > $2
