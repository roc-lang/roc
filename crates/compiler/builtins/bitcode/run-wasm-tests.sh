#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# Test failures will always point at the _start function
# Make sure to look at the rest of the stack trace!

# Zig will try to run the test binary it produced, but since your OS doesn't know how to
# run Wasm binaries natively, we need to provide a Wasm interpreter as a "test command".
zig test -target wasm32-wasi-musl -O ReleaseFast src/main.zig --test-cmd ../../../../target/release/roc_wasm_interp --test-cmd-bin
