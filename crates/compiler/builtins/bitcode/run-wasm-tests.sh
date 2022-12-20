#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# For non-native binaries, Zig test needs a "test command" it can use
cargo build --locked --release -p roc_wasm_interp
zig test -target wasm32-wasi-musl -O ReleaseFast src/main.zig --test-cmd ../../../../target/release/roc_wasm_interp --test-cmd-bin
