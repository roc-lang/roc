#!/usr/bin/env bash

set -euxo pipefail

zig build-exe benchmark/dec.zig -O ReleaseFast --main-pkg-path .
./dec
