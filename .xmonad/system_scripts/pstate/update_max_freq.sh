#!/bin/bash

freq=`$(dirname $0)/get_max_freq.sh`
$(dirname $0)/set_max_freq.sh $((freq + $1))
