#!/bin/bash

tcp_values=`cat /proc/net/snmp | grep "Tcp:" | head -n 2 | tail -n 1`
set -- $tcp_values

tcp_columns=`cat /proc/net/snmp | grep "Tcp:" | head -n 1`

for column in $tcp_columns; do
    if [ "$column" == "RetransSegs" ]; then
        echo $1
        break
    fi
    shift
done
