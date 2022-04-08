#!/bin/bash

set -euxo pipefail

# Test failures will always point at the _start function
# Make sure to look at the rest of the stack trace!
warning_about_non_native_binary=$(zig test -target wasm32-wasi-musl -O ReleaseFast src/main.zig 2>&1)
wasm_test_binary=$(echo $warning_about_non_native_binary | cut -d' ' -f 3)
wasmer $wasm_test_binary dummyArgForZigTestBinary
