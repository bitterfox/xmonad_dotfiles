#!/bin/bash

freq=`$(dirname $0)/get_min_freq.sh`
$(dirname $0)/set_min_freq.sh $((freq + $1))
