#!/bin/bash

set -euxo pipefail

# Test failures will always point at the _start function
# Make sure to look at the rest of the stack trace!

# Zig will try to run the test binary it produced, but it is a wasm object and hence your OS won't
# know how to run it. In the error message, it prints the binary it tried to run. We use some fun
# unix tools to get that path, then feed it to wasmer
warning_about_non_native_binary=$(zig test -target wasm32-wasi-musl -O ReleaseFast src/main.zig 2>&1) || true
wasm_test_binary=$(echo $warning_about_non_native_binary | cut -d' ' -f 35 )
wasmer $wasm_test_binary dummyArgForZigTestBinary
