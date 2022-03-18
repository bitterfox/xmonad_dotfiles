#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

~/scripts/open_I.sh print kibana > $2
