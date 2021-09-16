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
    items="`$basedir/one_password_list_items.sh`"
    if [ $? -ne 0 ]; then
        login
        items=`$basedir/one_password_list_items.sh`
    fi
    echo "$items" | fzf --preview 'op --cache get item {1} | jq' --preview-window=hidden \
                        --bind "ctrl-r:reload($basedir/one_password_list_items.sh --evict-cache)" \
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
