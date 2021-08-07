#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

in="$1"
out="$2"

history_path="$HOME/.config/vivaldi/Default/History"

cp $history_path /tmp

match=`sqlite3 /tmp/History '.separator "<>"' "select last_visit_time/1000000-11644473600,visit_count,typed_count,url,title from  urls order by last_visit_time desc" | awk -F'<>' '{print int(($2 + $3) / 10) " " $1 " " $4 "  :  " $5}' | sort -k 1nr,1 -k 2nr,2 | fzf --nth=3.. --tiebreak=index`

if [[ $? -eq 0 ]]; then
    echo $match | awk '{print $3}' > $out
fi

#match=`cat ~/.zsh_history | fzf`

#zsh -c "$match"
