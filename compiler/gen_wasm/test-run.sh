#!/bin/bash

TARGET_DIR=$1

if [[ -z "$TARGET_DIR" ]]
then
    echo "$0 needs an argument: target directory for output wasm and wat files"
    exit 1
fi

rm -rf output $TARGET_DIR
mkdir -p output $TARGET_DIR $TARGET_DIR/wasm $TARGET_DIR/wat
cargo test -- --test-threads=1 --nocapture

mv output/* $TARGET_DIR/wasm

for f in `ls $TARGET_DIR/wasm`
do
    wasm2wat $TARGET_DIR/wasm/$f -o $TARGET_DIR/wat/${f%.wasm}.wat
done


SIZE=$(du -b "$TARGET_DIR/wasm")
echo "Total bytes *.wasm = $SIZE"
