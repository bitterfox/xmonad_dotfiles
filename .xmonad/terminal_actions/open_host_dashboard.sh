#!/bin/zsh

export FZFLET_ACTIONS_DISABLED="false"
. "`dirname $0`/.terminal_action.rc"

~/scripts/open_host_dashboard.sh print > $2
