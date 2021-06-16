#!/bin/bash

set -euxo pipefail

zig build-exe benchmark/dec.zig --main-pkg-path .
./dec
