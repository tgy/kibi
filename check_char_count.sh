#!/bin/bash

# argument : the directory to recursively check
# output : a list of .ml files and line numbers where there are 80+ characters.

find $1 -regex .*\.ml$ |
while read f
do
    cat -n $f |
    while read l
    do
        if [ "$(echo "$l" | cut -f 2 | wc -m)" -gt 80 ]
        then
            echo -e "$f\t$(echo "$l" | cut -f 1)"
        fi
    done
done
