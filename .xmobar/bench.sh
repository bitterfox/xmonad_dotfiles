#/bin/bash
for i in `seq 0 50`; do
    $@ > /dev/null
done

