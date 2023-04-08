#!/bin/zsh

export FZFLET_ACTIONS_DISABLED="false"
. "`dirname $0`/.terminal_action.rc"

output="$2"

~/git-repos/github.com/bitterfox/fzflet/1password_v1/fzf_1password.sh "$output"
