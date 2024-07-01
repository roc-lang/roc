#!/bin/bash

ROC_PRINT_BUILD_COMMANDS=1

if [ ! -f "$PROJ/target/debug/roc" ]; then
    echo "roc must be built in debug mode before running this script."
    exit 1
fi

if [ -z "$PROJ" ]; then
    echo "PROJ must be set with the path to the project root."
    exit 1
fi

ZIG_VERSION="$(zig version)"
if [[ "$ZIG_VERSION" != "0.11.0" ]]; then
    echo "Zig version 0.11.0 is required."
    echo Version $ZIG_VERSION detected.
    exit 1
fi

LDD_VERSION=$(wasm-ld --version)
if [[ "$(sed 's/LDD \(\d+\)\..*/\1/' <<< $LDD_VERSION)" < 17 ]]; then
    echo "LLD version >= 17 is required."
    echo Version $LDD_VERSION detected.
    exit 1
fi

NODE_VERSION=$(node --version)
if [[ "$(sed 's/v\(\d+\)\..*/\1/' <<< $NODE_VERSION)" < 20 ]]; then
    echo "Node version >= 20 is required."
    echo Version $NODE_VERSION detected.
    exit 1
fi

set -xeuo pipefail

$PROJ/target/debug/roc build main.roc --target=wasm32 --optimize --no-link
#zig build-obj platform/host.zig -femit-llvm-ir=platform/main.bc --mod glue::$PROJ/crates/compiler/builtins/bitcode/src/glue.zig --deps glue --library c -target wasm32-wasi -fPIC -fstrip
#wasm-ld host.o main.wasm $PROJ/target/debug/lib/wasi-libc.a -o out.wasm --export-all --no-entry --import-undefined --export-table
#wasm-ld main.wasm -o out.wasm --export-all --no-entry --export-table --import-undefined 
wasm-ld main.wasm -o out.wasm --export-all --no-entry --export-table --import-undefined 
#cp main.wasm out.wasm
node nolink.js
