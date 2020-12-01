#!/bin/bash

set -eux

# Test every zig
find src/*.zig -type f -exec zig test --library c {} \;
