#!/usr/bin/env bash

result=`cat out.s | grep -Eoh '( [0-9a-f]{2}){1,3}' | awk '{ for(i = 1; i <= NF; i++) { print $i; } }'`
expected=`cat mario.hexdump | cut -d ' ' -f2-17 | awk '{ for(i = 1; i <= NF; i++) { print $i } }'`
echo "$result" > result.tmp
echo "$expected" > expected.tmp
i=1
while read -r e <&3 && read -r r <&4;
do
  if [ "$e" != "$r" ];
  then
    echo "expected $r and $e to be the same: line $i"
    exit 1
  fi
  i=$((i+1))
done 3<expected.tmp 4<result.tmp
rm -f expected.tmp result.tmp
echo "hexcode order test -- PASSED"
