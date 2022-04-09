#!/bin/bash

basedir=`dirname $0`

item_uuid=$1
export output=$2

item=`$basedir/one_password_get_item.sh $item_uuid`
fields=`jq '.fields' <<< $item`

if [ $? -ne 0 ]; then
item=`$basedir/one_password_get_item.sh --evict-cache $item_uuid`
fields=`jq '.fields' <<< $item`
fi

title=`jq -r '.title' <<< $item`
url=`jq -r '(.urls // []) | .[] | select(.primary) | .href' <<< $item`
tags=`jq -r '(.tags // []) | join(",")' <<< $item`

if [ -n "$tags" ]; then
    tags="[$tags] "
fi

show_fields() {
    show_fields_action "Fill"
    show_fields_action "Copy"
}

fill_username() {
    if [ -n "$username" ]; then
        echo "Fill username: $username"
    fi
}
fill_password() {
    if [ -n "$password" ]; then
        echo "Fill password: ***"
    fi
}

show_fields_action() {
    action="$1"
    i=0
    jq -c '.[]' <<< $fields | while read field; do
#        n=`jq -r '.id' <<< $field`
#        k=`jq -r '.type' <<< $field`
        value=`jq -r '((. | select(.type == "CONCEALED") | ((.value // "") | length)*"*") // .value) // ""' <<< $field`
#        if [ "$k" == "CONCEALED" ]; then
#            concealed_value=`jq -r '(.value | length)*"*"' <<< $field`
#            echo "$i $action $label: $value"
        if [ -n "$value" ]; then
            label=`jq -r '.label' <<< $field`
            echo "$i $action $label: $value"
        fi
        i=$((i+1))
    done
}

copy_username() {
    if [ -n "$username" ]; then
        echo "Copy username: $username"
    fi
}
copy_password() {
    if [ -n "$password" ]; then
        echo "Copy password"
    fi
}

header="$tags$title $url"
result=`show_fields | fzf --with-nth=2.. --preview "echo '$password'" --bind 'alt-p:toggle-preview' --preview-window=down:hidden --header "$header" --bind "ctrl-r:reload($basedir/one_password_get_item.sh --evict-cache $item_uuid)"`

#show_fields

if [ $? -ne 0 ]; then
    exit 1
fi

do_action() {
    id=$1
    action=$2

    value=`jq -r -c ".fields[$id].value // \"\"" <<< $item`

    if [ -n "$value" ]; then
        case "$action" in
            "Fill" ) echo -n "$value" > "$output" ;;
            "Copy" ) echo -n "$value" | xsel -b -i ;;
        esac
    fi
}

if [ -n "$result" ]; then
    id="`echo "$result" | awk '{print $1}'`"
    action="`echo "$result" | awk '{print $2}'`"

    do_action $id $action
fi

echo -n $item_uuid
