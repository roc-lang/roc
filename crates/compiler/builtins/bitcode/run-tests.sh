#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# Execute zig tests (see build.zig)
zig build test

# check formatting of zig files
find src/*.zig -type f -print0 | xargs -n 1 -0 zig fmt --check || (echo "zig fmt --check FAILED! Check the previous lines to see which files were improperly formatted." && exit 1)
