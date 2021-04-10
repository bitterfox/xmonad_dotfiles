#!/bin/zsh

in="$1"
out="$2"

. ~/.zshrc

project=`~/scripts/list_intellij_projects.sh 2020.2 100 | fzf`

if [ -n "$project" ]; then
    echo "$project" > $out
#    nohup ~/bin/idea "$project"
fi
