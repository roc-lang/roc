#!/bin/bash

if ! which wasm-bindgen > /dev/null
then
    cargo install wasm-bindgen-cli
fi

# Generate JS bindings for the combined app+host binary
SCRIPT_DIR=$(dirname "${BASH_SOURCE[0]}")
wasm-bindgen "${SCRIPT_DIR}/app/helloWeb.wasm" --target web --no-typescript --out-dir "${SCRIPT_DIR}/www"

# comment out a badly formatted import from wasm-bindgen
sed -i  's/^import/\/\/import/' "${SCRIPT_DIR}/www/helloWeb.js"
