#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

for file in crates/compiler/builtins/roc/*.roc; do
    if grep -qE '^\s*expect' "$file"; then
        cargo run --locked --release -- test  "$file"
    fi
done