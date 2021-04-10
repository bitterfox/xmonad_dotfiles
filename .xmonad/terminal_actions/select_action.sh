#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

cat $1 | fzf > $2
