#!/bin/bash

set -euxo pipefail

zig build-obj src/main.zig -O ReleaseFast -femit-llvm-ir=builtins.ll -femit-bin=builtins.o --strip
