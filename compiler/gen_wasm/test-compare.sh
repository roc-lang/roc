#!/bin/bash

if [[ -z "$1" || -z "$2" ]]
then
    echo "$0 needs 2 arguments: the directories to compare"
    exit 1
fi


OVERHEAD_BYTES=114 # total file size minus generated code size (test wrapper + module headers)


printf "filename                  \tLHS\tRHS\tchange\n"
printf "========                  \t===\t===\t======\n"

for f in `ls $1/wasm`
do
    if [[ ! -f "$2/wasm/$f" ]]
    then
        echo "$f found in $1/wasm but not in $2/wasm"
        continue
    fi
    SIZE1=$(stat --format '%s' "$1/wasm/$f")
    SIZE2=$(stat --format '%s' "$2/wasm/$f")
    CHANGE=$(( $SIZE2 - $SIZE1 ))
    NET_SIZE1=$(( $SIZE1 - $OVERHEAD_BYTES ))
    NET_SIZE2=$(( $SIZE2 - $OVERHEAD_BYTES ))
    PERCENT_CHANGE=$(( $CHANGE * 100 / $NET_SIZE1 ))
    printf "%s\t%d\t%d\t%d\t%d%%\n" $f $NET_SIZE1 $NET_SIZE2 $CHANGE $PERCENT_CHANGE
done
