#!/usr/bin/env bash
    
# Checks for:
# - zig pub declarations without doc comments

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euo pipefail

found_errors=false

# Lint all zig files in src/
while read -r file; do
    errors=$(awk '
        /^pub / {
            if (prev !~ /^\/\/\//) {
                print FILENAME ":" FNR ": pub declaration without doc comment `///`"
            }
        }
        { prev = $0 }
    ' "$file")
    if [[ -n "$errors" ]]; then
        echo "$errors"
        found_errors=true
    fi
done < <(find src -type f -name "*.zig")

if [[ "$found_errors" == true ]]; then
    echo ""
    echo "Please add doc comments to the spots listed above, they make the code easier to understand for everyone."
    exit 1
else
    echo "All pub declarations have doc comments."
fi