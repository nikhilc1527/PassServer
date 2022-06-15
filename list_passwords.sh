#!/bin/sh

tree -fFi --noreport ~/.local/share/pass | grep -v "/$" | awk -F/ '{printf "%s/%s\n", $(NF-1), $NF}' | rev | cut --complement -c -4 | rev | while read -r line ; do
  password=$(pass "$line" 2>/dev/null)
  if [ "$?" != 0 ]; then
    line=$(echo "$line" | awk -F/ '{print $NF}')
    password=$(pass "$line")
  fi
  printf "%s:%s\n" "$line" "$password"
done | grep -v "is not in the password store"
