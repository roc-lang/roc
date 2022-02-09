#!/bin/bash

set -eux

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

# Copy the JS from wasm_bindgen, replacing its invalid `import` statement with a `var`.
# The JS import from the invalid path 'env', seems to be generated when there are unresolved symbols.
BINDGEN_FILE="roc_repl_wasm.js"
echo 'var __wbg_star0 = { now: Date.now };' > $WWW_DIR/$BINDGEN_FILE
grep -v '^import' repl_wasm/pkg/$BINDGEN_FILE >> $WWW_DIR/$BINDGEN_FILE
