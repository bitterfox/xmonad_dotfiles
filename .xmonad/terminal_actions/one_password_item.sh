#!/bin/bash

basedir=`dirname $0`

item_uuid=$1
output=$2

#item=`op --cache get item $item_uuid`
item=`$basedir/one_password_get_item.sh $item_uuid`

username=`jq -r '.details.fields[] | select(.name == "username") | .value' <<< $item`

if [ $? -ne 0 ]; then
item=`$basedir/one_password_get_item.sh --evict-cache $item_uuid`
username=`jq -r '.details.fields[] | select(.name == "username") | .value' <<< $item`
fi

password=`jq -r '.details.fields[] | select(.name == "password") | .value' <<< $item`

title=`jq -r '.overview.title' <<< $item`
url=`jq -r '.overview.url' <<< $item`
tags=`jq -r '.overview.tags | join(",")' <<< $item`

if [ -n "$tags" ]; then
    tags="[$tags] "
fi

header="$tags$title $url"
result=`cat <<EOF | fzf --preview "echo '$password'" --bind 'alt-p:toggle-preview' --preview-window=down:hidden --header "$header" --bind 'ctrl-r:reload($basedir/one_password_get_item.sh $item_uuid)'
Fill username: $username
Fill password
Copy username: $username
Copy password
EOF`

if [ $? -ne 0 ]; then
    exit 1
fi

case "$result" in
    "Fill username: $username" ) echo -n "$username" > "$output" ;;
    "Fill password" ) echo -n "$password" > "$output" ;;
    "Copy username: $username" ) echo -n "$username" | xsel -b -i ;;
    "Copy password" ) echo -n "$password" | xsel -b -i ;;
esac

echo -n $item_uuid
