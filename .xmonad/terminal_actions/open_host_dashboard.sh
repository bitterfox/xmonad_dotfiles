#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

~/scripts/open_host_dashboard.sh print > $2
