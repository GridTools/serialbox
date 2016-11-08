#!/bin/bash

if [ "$#" -lt 2 ]; then 
    echo "usage: $0 <old> <new>"
    exit 1
fi

OLD="$1"
NEW="$2"

echo "Replacing '$OLD' with '$NEW'"

DPATH=$(find ./examples/ ./src/ ./test/ -name '*.c' -o -name '*.h' -o -name '*.cpp')
TFILE="/tmp/out.tmp.$$"

for f in $DPATH
do
  if [ -f $f -a -r $f ]; then
    sed "s|${OLD}|${NEW}|g" "$f" > $TFILE && mv $TFILE "$f"
  else
    echo "Error: Cannot read $f"
  fi
done

