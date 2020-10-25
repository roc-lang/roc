#!/bin/bash

set -euxo pipefail

# Clear out any existing output files
rm -rf ./out
mkdir ./out

# Regenerate the .bc file
clang -emit-llvm -o out/lib.bc -c src/lib.c

# Copy bc file for it to be used
cp ./out/lib.bc "$1"
