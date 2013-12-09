#!/bin/bash
for i in `seq $1 $2`
do
  ./test.native 2> /dev/null
  cp -v weights.txt "weights/weights${i}.txt"
done
