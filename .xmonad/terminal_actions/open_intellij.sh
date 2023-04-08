#!/bin/zsh

export FZFLET_ACTIONS_DISABLED="false"
. "`dirname $0`/.terminal_action.rc"

in="$1"
out="$2"

project=`$HOME/git-repos/github.com/bitterfox/fzflet/intellij/fzf_intellij_recent_projects.sh`

if [ -n "$project" ]; then
    echo "$project" > $out
    echo "$project" > /tmp/oi.debug
#    nohup ~/bin/idea "$project"
fi
