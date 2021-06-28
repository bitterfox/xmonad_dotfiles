#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

in="$1"
out="$2"

project=`~/scripts/list_intellij_projects.sh 2021.1 100 | fzf`

if [ -n "$project" ]; then
    echo "$project" > $out
#    nohup ~/bin/idea "$project"
fi
