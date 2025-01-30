#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euo pipefail

# Extract vars from .cargo/config.toml
config_vars=$(grep -E "^ROC_.*= \"[01]\"" .cargo/config.toml | cut -d'=' -f1 | tr -d ' ')

# Extract vars from crates/compiler/debug_flags/src/lib.rs
lib_vars=$(grep -E "^    ROC_.*" crates/compiler/debug_flags/src/lib.rs | tr -d ' ')

# Sort both lists
sorted_config_vars=$(echo "$config_vars" | sort)
sorted_lib_vars=$(echo "$lib_vars" | sort)

# Compare the sorted lists
if diff <(echo "$sorted_config_vars") <(echo "$sorted_lib_vars") > /dev/null; then
    echo "The flags in both files are identical."
else
    echo "Looks like some flags are out of sync between .cargo/config.toml and crates/compiler/debug_flags/src/lib.rs:"
    diff <(echo "$sorted_config_vars") <(echo "$sorted_lib_vars")
fi