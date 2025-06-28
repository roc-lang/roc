#!/bin/bash

# This script simulates AFL++ fuzzing by repeatedly calling the repro executable
# with inputs from the corpus directory and some mutations

set -e

echo "=== Simulating AFL++ Fuzzing for Canonicalization ==="
echo "This script mimics what AFL++ would do by feeding various inputs to the fuzzer"
echo ""

# Build the repro executable
echo "Building repro-canonicalize..."
zig build repro-canonicalize || exit 1

CORPUS_DIR="src/fuzz-corpus/canonicalize"
OUTPUT_DIR="/tmp/canonicalize-out"
CRASHES_DIR="$OUTPUT_DIR/crashes"
HANGS_DIR="$OUTPUT_DIR/hangs"

# Create output directories
mkdir -p "$CRASHES_DIR" "$HANGS_DIR"

echo "Corpus directory: $CORPUS_DIR"
echo "Output directory: $OUTPUT_DIR"
echo ""

# Function to run a test case
run_test() {
    local input_file="$1"
    local test_name="$2"

    # Run with timeout to detect hangs
    if gtimeout 2s bash -c "cat '$input_file' | ./zig-out/bin/repro-canonicalize" >/dev/null 2>&1; then
        echo -n "."  # Success
    else
        local exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo ""
            echo "HANG detected with: $test_name"
            cp "$input_file" "$HANGS_DIR/$(date +%s)_$test_name"
        else
            echo ""
            echo "CRASH detected with: $test_name (exit code: $exit_code)"
            cp "$input_file" "$CRASHES_DIR/$(date +%s)_$test_name"
        fi
    fi
}

# Test corpus files
echo "Testing corpus files..."
for file in "$CORPUS_DIR"/*; do
    if [ -f "$file" ] && [[ ! "$file" =~ README ]]; then
        run_test "$file" "$(basename "$file")"
    fi
done
echo ""

# Generate some mutations
echo "Generating and testing mutations..."
TEMP_FILE="/tmp/fuzz_input_$$"

# Mutation 1: Empty module
echo "module []" > "$TEMP_FILE"
run_test "$TEMP_FILE" "empty_module"

# Mutation 2: Module with unicode
echo "module [∞]" > "$TEMP_FILE"
run_test "$TEMP_FILE" "unicode_module"

# Mutation 3: Very long identifier
echo "module [x]" > "$TEMP_FILE"
echo "" >> "$TEMP_FILE"
echo "x = $(python3 -c 'print("a" * 1000)')" >> "$TEMP_FILE"
run_test "$TEMP_FILE" "long_identifier"

# Mutation 4: Deeply nested expression
echo "module [x]" > "$TEMP_FILE"
echo "" >> "$TEMP_FILE"
echo -n "x = " >> "$TEMP_FILE"
for i in {1..100}; do echo -n "(" >> "$TEMP_FILE"; done
echo -n "42" >> "$TEMP_FILE"
for i in {1..100}; do echo -n ")" >> "$TEMP_FILE"; done
run_test "$TEMP_FILE" "deeply_nested"

# Mutation 5: Binary data after valid header
echo "module [x]" > "$TEMP_FILE"
echo "" >> "$TEMP_FILE"
dd if=/dev/urandom bs=1 count=100 >> "$TEMP_FILE" 2>/dev/null
run_test "$TEMP_FILE" "binary_after_header"

# Mutation 6: Recursive definition
echo "module [f]" > "$TEMP_FILE"
echo "" >> "$TEMP_FILE"
echo "f = f" >> "$TEMP_FILE"
run_test "$TEMP_FILE" "recursive_def"

# Mutation 7: Many definitions
echo "module [x]" > "$TEMP_FILE"
echo "" >> "$TEMP_FILE"
for i in {1..1000}; do
    echo "x$i = $i" >> "$TEMP_FILE"
done
run_test "$TEMP_FILE" "many_definitions"

# Clean up
rm -f "$TEMP_FILE"

echo ""
echo ""
echo "=== Fuzzing Simulation Complete ==="
echo ""

# Report results
CRASH_COUNT=$(find "$CRASHES_DIR" -type f | wc -l | tr -d ' ')
HANG_COUNT=$(find "$HANGS_DIR" -type f | wc -l | tr -d ' ')

echo "Results:"
echo "  Crashes found: $CRASH_COUNT"
echo "  Hangs found: $HANG_COUNT"
echo ""

if [ $CRASH_COUNT -gt 0 ]; then
    echo "Crash files saved in: $CRASHES_DIR"
    echo "To debug a crash, run:"
    echo "  ./zig-out/bin/repro-canonicalize --verbose <crash_file>"
fi

if [ $HANG_COUNT -gt 0 ]; then
    echo "Hang files saved in: $HANGS_DIR"
fi

echo ""
echo "Note: This is a simulation. Real AFL++ fuzzing would:"
echo "  - Generate many more test cases"
echo "  - Use coverage feedback to guide mutations"
echo "  - Run continuously for hours/days"
echo "  - Be much more effective at finding bugs"
