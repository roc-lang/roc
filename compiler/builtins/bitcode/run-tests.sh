#!/bin/bash

set -euxo pipefail

# Test every zig
find src/*.zig -type f -print0 | xargs -n 1 -0 zig test --library c

# fmt every zig
find src/*.zig -type f -print0 | xargs -n 1 -0 zig fmt --check
