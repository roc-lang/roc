#!/bin/bash

set -ex

app_wasm="generated/app.wasm"
app_include="generated/app_bytes.c"
app_rust="src/generated_app_bytes.rs"
comp_wasm="dist/compiler.wasm"

mkdir -p generated dist
rm -f generated/* dist/*

zig9 build-lib -target wasm32-freestanding-musl -dynamic src/app.c -femit-bin=$app_wasm

./print_bytes_as_rust_code.js $app_wasm > $app_rust

# wasm-pack build --target web --dev
wasm-pack build --target web --release

cp src/index.html dist
cp src/repl.js dist
cp pkg/mock_repl_bg.wasm dist
cp pkg/mock_repl.js dist
