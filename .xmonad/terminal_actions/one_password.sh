#!/bin/zsh

. ~/.zshrc
. "`dirname $0`/.terminal_action.rc"

output="$2"

basedir=`dirname $0`

login() {
    OP_SESSION_my=""
    while [ -z "$OP_SESSION_my" ]; do
        eval $(op signin my)
    done
}

session_path="/tmp/last_op_session_${USER}"
if [ -f $session_path ]; then
    . $session_path
else
    touch $session_path
    chmod 600 $session_path
fi

SESSION_EXPIRE=${SESSION_EXPIRE:-0}

if [ $SESSION_EXPIRE -lt `date +%s` ]; then
    echo "session expired"
    login

    LAST_ITEM=""
fi

SESSION_EXPIRE=`date -d "10 minutes" +%s`

run() {
    items=`op list items`
    if [ $? -ne 0 ]; then
        login
        items=`op list items`
    fi
    items=`jq -r '.[] | (.uuid + " " + .overview.title + " " + .overview.ainfo + " " + .overview.url)' <<< $items`
    echo "$items" | fzf --preview 'op get item {1} | jq' \
                        --bind "enter:execute-and-exit-on-success($basedir/one_password_item.sh {1} $output)" \
                        --with-nth=2..
}

if [ -n "$LAST_ITEM" ]; then
    i="`$basedir/one_password_item.sh "$LAST_ITEM" "$output"`"
    if [ $? -ne 0 ]; then
        i="`run`"
    fi
else
    i="`run`"
fi

echo $i

cat > $session_path <<EOF
export SESSION_EXPIRE="$SESSION_EXPIRE"
export OP_SESSION_my="$OP_SESSION_my"
export LAST_ITEM="$i"
EOF
