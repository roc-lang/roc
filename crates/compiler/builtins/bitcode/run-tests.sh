#!/bin/bash

set -euxo pipefail

# Test every zig
zig build test

# fmt every zig
find src/*.zig -type f -print0 | xargs -n 1 -0 zig fmt --check || (echo "zig fmt --check FAILED! Check the previous lines to see which files were improperly formatted." && exit 1)
