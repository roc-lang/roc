#!/bin/bash

set -euxo pipefail

# Clear out any existing output files. Sometimes if these are there, rustc
# doesn't generate the .bc file - or we can end up with more than one .bc
rm -rf ../../../target/release/deps/

# Regenerate the .bc file
cargo rustc --release --lib -- --emit=llvm-bc

bc_files=$(ls ../../../target/release/deps/*.bc | wc -l)

if [[ $bc_files != 1 ]]; then
  echo "More than one .bc file was emitted somehow."
  exit 1;
fi

cp ../../../target/release/deps/*.bc ../../gen/src/llvm/builtins.bc
