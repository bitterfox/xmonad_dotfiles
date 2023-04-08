#!/bin/zsh

export FZFLET_ACTIONS_DISABLED="false"
. "`dirname $0`/.terminal_action.rc"

in="$1"
out="$2"

sortkey=`cat $in`

match=`~/git-repos/github.com/bitterfox/fzflet/google-chrome/fzf_google-chrome_history.sh $sortkey`

if [[ $? -eq 0 ]]; then
    echo $match | awk '{print $3}' > $out
fi

#match=`cat ~/.zsh_history | fzf`

#zsh -c "$match"
