#!/bin/bash

export FZFLET_ACTIONS_DISABLED="false"
. "`dirname $0`/.terminal_action.rc"

in="$1"
out="$2"

match=`curl -s http://localhost:9222/json | jq -r '.[] | "\(.id)\t\(.title)\t\(.url)"' | fzf | awk '{print $1}'`

if [[ $? -eq 0 ]]; then
    curl http://localhost:9222/json/activate/$match
fi
