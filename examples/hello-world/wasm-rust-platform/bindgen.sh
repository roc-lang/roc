#!/bin/bash

if ! which wasm-bindgen > /dev/null
then
    cargo install wasm-bindgen
fi

# Generate JS bindings for the combined app+host binary
wasm-bindgen roc/helloWeb.wasm --target web --no-typescript --out-dir www

# comment out a badly formatted import from wasm-bindgen
sed -i  's/^import/\/\/import/' www/helloWeb.js
