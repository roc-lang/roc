#!/bin/bash

# Quick script to test the type-checking code generator with random inputs
# Usage: ./test-typecheck-random.sh [count]
#   count: number of random samples to generate (default: 5)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
REPRO_BIN="$REPO_ROOT/zig-out/bin/repro-typecheck"

# Check if repro binary exists
if [ ! -f "$REPRO_BIN" ]; then
    echo "repro-typecheck not found. Building..."
    cd "$REPO_ROOT"
    zig build
fi

COUNT=${1:-5}

echo "Generating $COUNT random type-checked Roc modules..."
echo

errors=0
for i in $(seq 1 $COUNT); do
    # Generate random bytes
    RANDOM_DATA=$(head -c 16 /dev/urandom | base64)

    echo "════════════════════════════════════════════════════════"
    echo "Sample $i"
    echo "════════════════════════════════════════════════════════"

    result=$(echo -n "$RANDOM_DATA" | "$REPRO_BIN" --verbose 2>&1)
    echo "$result"

    # Check for errors
    if echo "$result" | grep -qE "TYPE MISMATCH|COMPTIME CRASH|panic|UNEXPECTED"; then
        errors=$((errors + 1))
        echo ""
        echo "⚠️  ERROR DETECTED"
    fi
    echo
done

echo "════════════════════════════════════════════════════════"
echo "Summary: $errors errors out of $COUNT samples"
echo "════════════════════════════════════════════════════════"

if [ $errors -gt 0 ]; then
    exit 1
fi
