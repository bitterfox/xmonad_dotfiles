#!/bin/zsh

export FZFLET_ACTIONS_DISABLED="false"
. "`dirname $0`/.terminal_action.rc"

~/scripts/open_I.sh print > $2
