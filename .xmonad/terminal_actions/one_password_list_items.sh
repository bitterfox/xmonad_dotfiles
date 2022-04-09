#!/bin/bash

set -e

evict_cache=0
if [ "$1" == "--evict-cache" ]; then
    evict_cache=1
fi

CACHE_DIR=~/.cache/xmonad/one_password/

if [ ! -d $CACHE_DIR ]; then
  mkdir -p $CACHE_DIR
  chmod 700 $CACHE_DIR
fi

CACHE_FILE=$CACHE_DIR/list_items

if [ $evict_cache -eq 1 ] || [ ! -s $CACHE_FILE ]; then
    rm $CACHE_FILE > /dev/null 2>&1 || true
fi

if [ ! -f $CACHE_FILE ]; then
    op item list
    read
    items=`op item list --format=json`
    cat > $CACHE_FILE <<< $items
fi

len_title=`cat $CACHE_FILE | jq -r "[.[] | .title | length] | max"`
#len_ainfo=`cat $CACHE_FILE | jq -r "[ .[] | .overview.ainfo | length ] | max"`

cat $CACHE_FILE | jq -r ".[] | (.id + \" \" + .title + ((1+$len_title - (.title | length))*\" \"))" | sort -k 2
