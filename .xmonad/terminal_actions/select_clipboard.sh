#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

# CM_DIR=$HOME clipmenu_fzf --preview-window="down:30%"
file=`CM_DIR=$HOME ~/git-repos/github.com/bitterfox/fzflet/clipmenu/fzf_clipmenu_list.sh`
#file=`CM_DIR=$HOME ~/git-repos/github.com/bitterfox/fzflet/clipmenu/fzf_clipmenu_ag.sh`

root=`CM_DIR=$HOME clipctl cache-dir`

for selection in clipboard primary; do
    xsel --logfile /dev/null -i --"$selection" < "$root/$file"
done

