#!/bin/bash

set -ex

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

# For debugging, pass the --profiling option, which enables optimizations + debug info
# (We need optimizations to get rid of dead code that otherwise causes compile errors!)
if [ -n "$REPL_DEBUG" ]
then
    cargo build --target wasm32-unknown-unknown -p roc_repl_wasm --release
    wasm-bindgen --target web --keep-debug target/wasm32-unknown-unknown/release/roc_repl_wasm.wasm --out-dir repl_wasm/pkg/
else
    wasm-pack build --target web repl_wasm
fi

cp repl_wasm/pkg/*.wasm $WWW_DIR

# Copy the JS from wasm_bindgen, replacing its invalid `import` statement with a `var`.
# The JS import from the invalid path 'env', seems to be generated when there are unresolved symbols.
BINDGEN_FILE="roc_repl_wasm.js"
echo 'var __wbg_star0 = { now: Date.now };' > $WWW_DIR/$BINDGEN_FILE
grep -v '^import' repl_wasm/pkg/$BINDGEN_FILE >> $WWW_DIR/$BINDGEN_FILE
