#!/bin/bash

if [[ ! -d repl_www ]]
then
    echo "This script should be run from the project root"
    exit 1
fi

if ! which wasm-pack
then
    cargo install wasm-pack
fi

WWW_DIR="repl_www/build"
mkdir -p $WWW_DIR
cp repl_www/public/* $WWW_DIR

# Pass all script arguments through to wasm-pack (such as --release)
wasm-pack build --target web "$@" repl_wasm

cp repl_wasm/pkg/*.wasm $WWW_DIR
cp repl_wasm/pkg/*.js $WWW_DIR
