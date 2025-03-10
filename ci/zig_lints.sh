#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euo pipefail

# Check for pub declarations without doc comments

found_errors=false

# Lint all zig files in src/
while read -r file; do
    errors=$(awk '
        /^pub / {
            if (prev !~ /^\/\/\//) {
                # Only init and deinit functions should not have doc comments
                if ($0 !~ /pub.*fn init\(/ && $0 !~ /pub.*fn deinit/) {
                    print FILENAME ":" FNR ": pub declaration without doc comment `///`"
                }
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
    echo ""
    exit 1
fi

# Check for top level comments in new Zig files

NEW_ZIG_FILES=$(git diff --name-only --diff-filter=A origin/main HEAD | grep 'src/' | grep '\.zig$' || echo "")

if [ -z "$NEW_ZIG_FILES" ]; then
    # No new Zig files found
    exit 0
fi

FAILED_FILES=""

for FILE in $NEW_ZIG_FILES; do
    if ! grep -q "//!" "$FILE"; then
    echo "Error: $FILE is missing top level comment (//!)"
    FAILED_FILES="$FAILED_FILES $FILE"
    fi
done

if [ -n "$FAILED_FILES" ]; then
    echo ""
    echo "The following files are missing a top level comment:"
    echo "    $FAILED_FILES"
    echo ""
    echo "Add a //! comment BEFORE any other code that explains the purpose of the file."
    exit 1
fi