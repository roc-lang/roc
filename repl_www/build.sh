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

# When debugging the REPL, use `REPL_DEBUG=1 repl_www/build.sh`
if [ -n "${REPL_DEBUG:-}" ]
then
    # Leave out wasm-opt since it takes too long when debugging, and provide some debug options
    cargo build --target wasm32-unknown-unknown -p roc_repl_wasm --release --features console_error_panic_hook
    wasm-bindgen --target web --keep-debug target/wasm32-unknown-unknown/release/roc_repl_wasm.wasm --out-dir repl_wasm/pkg/
else
    # A `--profiling` build is optimized and has debug info, so we get stack traces for compiler `todo!()`
    wasm-pack build --profiling --target web repl_wasm -- --features console_error_panic_hook -v
fi

cp repl_wasm/pkg/*.wasm $WWW_ROOT

# Copy the JS from wasm_bindgen, replacing its invalid `import` statement with a `var`.
# The JS import from the invalid path 'env', seems to be generated when there are unresolved symbols.
BINDGEN_FILE="roc_repl_wasm.js"
echo 'var __wbg_star0 = { now: Date.now };' > $WWW_ROOT/$BINDGEN_FILE
grep -v '^import' repl_wasm/pkg/$BINDGEN_FILE >> $WWW_ROOT/$BINDGEN_FILE
