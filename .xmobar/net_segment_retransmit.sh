#!/bin/bash

tcp_columns=`cat /proc/net/snmp | grep "Tcp:" | head -n 1 | sed -r "s/Tcp: (.*)/\1/"`
tcp_values=`cat /proc/net/snmp | grep "Tcp:" | head -n 2 | tail -n 1 | sed -r "s/Tcp: (.*)/\1/"`

#echo $tcp_columns
#echo $tcp_values

while [ -n "$tcp_columns" ]; do
    if [ "`echo $tcp_columns | sed -r "s/([^ ]+)(.*)/\1/"`" == "RetransSegs" ]; then
        echo $tcp_values | sed -r "s/([^ ]+)(.*)/\1/"
    fi
    tcp_columns=`echo $tcp_columns | sed -r "s/([^ ]+)(.*)/\2/"`
    tcp_values=`echo $tcp_values | sed -r "s/([^ ]+)(.*)/\2/"`
done
