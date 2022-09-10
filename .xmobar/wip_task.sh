#!/bin/bash

if [ ! -f ~/git-repos/github.com/bitterfox/fzflet/jira/jira_worklog_preview_task.sh ]; then
    exit 0
fi

str=`~/git-repos/github.com/bitterfox/fzflet/jira/jira_worklog_preview_task.sh`
if [ -z "$str" ]; then
    echo ""
    exit 0
fi

max_len=40
str_len=`expr length "$str"`
ticket=`echo "$str" | awk '{print $1}'`
if [ $str_len -le $max_len ]; then
    echo "⏳$str"
    exit 0
fi

task=`echo "$str" | sed -r "s/(.*) \((.*)\)/\1/"`
time=`echo "$str" | sed -r "s/(.*) \((.*)\)/... (\2)/"`

time_len=`expr length "$time"`
task=`echo "$str" | cut -c -$((max_len - time_len))`

echo "⏳$task$time"
