#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

cat $1 | fzf --with-nth=2.. --header-lines=1 | awk '{print $1}'> $2
