#!/usr/bin/env bash

# This script
#   1. Uses cargo to build the Roc compiler as a .wasm file (build.rs is part of this step only)
#   2. Further optimizes the .wasm, and generates JavaScript code to interface with it
#
# After running this, we zip the generated assets and host them somewhere public on the web.
# Our website deployment script downloads that zipfile and copies the files into www/build/repl/
# We use this two-step process because Netlify times out if we try to build the Web REPL there.

set -euxo pipefail

if ! which wasm-pack
then
    echo "To build the Web REPL, you need to run 'cargo install wasm-pack'"
    exit 1
fi

SCRIPT_RELATIVE_DIR=$(dirname "${BASH_SOURCE[0]}")
cd $SCRIPT_RELATIVE_DIR

mkdir -p build
rm -rf build/*

# We want a release build, but with debug info (to get stack traces for Wasm backend panics)
# This configuration is called `--profiling`
wasm-pack build --profiling --target web -- --features console_error_panic_hook
cp -v pkg/roc_repl_wasm.js build

# To disable optimizations while debugging, do `export REPL_DEBUG=1` before running the script
if [ "${REPL_DEBUG:-}" == "" ] && which wasm-opt
then
    wasm-opt -Os --debuginfo pkg/roc_repl_wasm_bg.wasm -o build/roc_repl_wasm_bg.wasm
else
    echo "wasm-opt is not installed. Skipping .wasm optimization."
    cp -v pkg/roc_repl_wasm_bg.wasm build
fi

# Copy the JS from wasm_bindgen, replacing its invalid `import` statement with a `var`.
# The JS import from the invalid path 'env', seems to be generated when there are unresolved symbols.
BINDGEN_FILE="roc_repl_wasm.js"
echo 'var __wbg_star0 = { now: Date.now };' > build/$BINDGEN_FILE
grep -v '^import' pkg/$BINDGEN_FILE >> build/$BINDGEN_FILE

# As of July 2022, the .wasm file is ~4MB, shrinking to ~1MB with Brotli compression (which Netlify does)
echo "Generated REPL assets for website:"
ls -l build

TARFILE=${1:-roc_repl_wasm.tar.gz}
cd build
tar cvzf $TARFILE *
