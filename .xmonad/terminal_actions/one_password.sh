#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

output="$2"

~/git-repos/github.com/bitterfox/fzflet/1password_v1/fzf_1password.sh "$output"
