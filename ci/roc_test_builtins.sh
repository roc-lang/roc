#!/usr/bin/env bash

for file in crates/compiler/builtins/roc/*.roc; do
    if grep -qE '^\s*expect' "$file"; then
        cargo run --locked --release -- test  "$file"
    fi
done