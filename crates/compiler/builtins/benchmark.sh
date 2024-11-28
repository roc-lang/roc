#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

zig build-exe benchmark-dec.zig -O ReleaseFast
./benchmark-dec
