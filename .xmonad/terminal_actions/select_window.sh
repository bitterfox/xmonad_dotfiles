#!/bin/zsh

export FZFLET_ACTIONS_DISABLED="false"
. "`dirname $0`/.terminal_action.rc"

#cat $1 | sort -k 2,2 | fzf --with-nth=2.. --header-lines=1 | awk '{print $1}'> $2
# cat $1 | fzf --with-nth=2.. --header-lines=1 --bind 'change:' | awk '{print $1}'> $2
current=`cat $1 | head -n 1 | awk '{print $1}'`
current_line=`grep -n "$current" $1 | head -n 2 | tail -n 1 | awk -F: '{print $1}'`

# Skipt first line, so -1
current_line=$((current_line - 1))

if [ $current_line -lt 0 ]; then
    current_line=0
fi

cat $1 | fzf --header-lines=1 --with-nth=2.. --bind 'change:' --bind "load:pos($current_line)" --scheme=history | awk '{print $1}'> $2
