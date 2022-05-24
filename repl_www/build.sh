#!/usr/bin/env bash

set -euxo pipefail

if [[ ! -d repl_www ]]
then
    echo "This script should be run from the project root"
    exit 1
fi

if ! which wasm-pack
then
    echo "Installing wasm-pack CLI"
    cargo install wasm-pack
fi

# output directory is first argument or default
WWW_ROOT="${1:-repl_www/public}"
mkdir -p $WWW_ROOT

# We want a release build, but with debug info (to get stack traces for Wasm backend `todo!()`)
# This configuration is called `--profiling`
wasm-pack build --profiling --target web repl_wasm -- --features console_error_panic_hook -v
cp -v repl_wasm/pkg/roc_repl_wasm.js $WWW_ROOT

# To disable optimizations while debugging, run `REPL_DEBUG=1 repl_www/build.sh`
if [ "${REPL_DEBUG:-}" == "" ] && which wasm-opt
then
    wasm-opt -Os --debuginfo repl_wasm/pkg/roc_repl_wasm_bg.wasm -o $WWW_ROOT/roc_repl_wasm_bg.wasm
else
    echo "wasm-opt is not installed. Skipping .wasm optimization."
    cp -v repl_wasm/pkg/roc_repl_wasm_bg.wasm $WWW_ROOT
fi

# Copy the JS from wasm_bindgen, replacing its invalid `import` statement with a `var`.
# The JS import from the invalid path 'env', seems to be generated when there are unresolved symbols.
BINDGEN_FILE="roc_repl_wasm.js"
echo 'var __wbg_star0 = { now: Date.now };' > $WWW_ROOT/$BINDGEN_FILE
grep -v '^import' repl_wasm/pkg/$BINDGEN_FILE >> $WWW_ROOT/$BINDGEN_FILE
