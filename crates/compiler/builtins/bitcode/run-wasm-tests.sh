#!/usr/bin/env bash

set -euxo pipefail

# Test failures will always point at the _start function
# Make sure to look at the rest of the stack trace!

# Zig will try to run the test binary it produced, but it is a wasm object and hence your OS won't
# know how to run it. In the error message, it prints the binary it tried to run. We use some fun
# unix tools to get that path, then feed it to wasmer
zig test -target wasm32-wasi-musl -O ReleaseFast src/main.zig --test-cmd wasmer --test-cmd-bin
