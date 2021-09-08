#!/bin/bash

factor=${1:-20}

current="`$(dirname $0)/get_current.sh`"
max="`$(dirname $0)/get_max.sh`"
new=$(( current + max / factor ))

fifth_percentile=$(( max / 20 ))
if [ $new -lt $fifth_percentile ]; then
  i=$((max / 100))
  if [ $i -lt 1 ]; then
    i=1
  fi
  new=$(( current - $i ))
fi

$(dirname $0)/set.sh $new
