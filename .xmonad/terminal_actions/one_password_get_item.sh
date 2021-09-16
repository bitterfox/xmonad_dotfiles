#!/bin/bash

set -o pipefail
set -e

evict_cache=0
if [ "$1" == "--evict-cache" ]; then
    evict_cache=1
    shift
fi

uuid=$1

CACHE_DIR=~/.cache/xmonad/one_password/items

if [ ! -d $CACHE_DIR ]; then
  mkdir -p $CACHE_DIR
  chmod 700 $CACHE_DIR
fi

CACHE_FILE=$CACHE_DIR/$uuid

if [ $evict_cache -eq 1 ] || [ ! -s $CACHE_FILE ]; then
    rm $CACHE_FILE 2>&1 > /dev/null || true
fi

if [ ! -f $CACHE_FILE ]; then
    item=`op get item $uuid`
    cat > $CACHE_FILE <<< $item
fi

cat $CACHE_FILE
