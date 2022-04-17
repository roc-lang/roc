#!/bin/bash

if ! which wasm-bindgen
then
    cargo install wasm-bindgen
fi

# Generate JS bindings for the combined app+host binary
wasm-bindgen roc/helloWeb.wasm --target web --out-dir www
