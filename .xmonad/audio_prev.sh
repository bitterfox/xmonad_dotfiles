#/bin/sh

indices=`pacmd list-sinks | grep index`
next_index=`echo "$indices\n$indices" | grep -B1 '*' | grep -v '*' | grep index | head -n 1 | sed -r "s/[^0-9]+([0-9]+)/\1/"`

while :; do
  echo "Setting $next_index"
  pacmd set-default-sink $next_index
  new_indices=`pacmd list-sinks | grep index`
  index=`echo "$new_indices" | grep '*' | sed -r "s/[^0-9]+([0-9]+)/\1/"`

  if [ "$next_index" = "$index" ]; then
    echo "Update input to $next_index"
    pacmd list-sink-inputs | grep index | sed -r "s/[^0-9]+([0-9]+)/\1/" | xargs -n1 -I% pacmd move-sink-input % $next_index
    break
  else
    echo "Wrong sink $next_index != $index"
    next_index=`echo "$indices\n$indices" | grep -B1 "index: $next_index" | grep -v "index: $next_index" | head -n 1 | sed -r "s/[^0-9]+([0-9]+)/\1/"`
  fi
done
