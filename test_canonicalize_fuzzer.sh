#!/bin/bash

# Test script for canonicalize fuzzer
# This simulates what AFL++ would do by feeding various inputs to the fuzzer

echo "Testing canonicalize fuzzer with various inputs..."
echo "============================================="

# Build the repro executable
echo "Building repro-canonicalize..."
zig build repro-canonicalize || exit 1

echo ""
echo "Testing with valid inputs (should pass canonicalization):"
echo ""

# Test 1: Simple module
echo "Test 1: Simple module"
printf 'module [x]\n\nx = 42' | ./zig-out/bin/repro-canonicalize --verbose
echo ""

# Test 2: Module with function
echo "Test 2: Module with function"
printf 'module [add]\n\nadd = \\a, b -> a + b' | ./zig-out/bin/repro-canonicalize --verbose
echo ""

# Test 3: App module
echo "Test 3: App module"
printf 'app [main] { pf: platform "../cli/platform.roc" }\n\nmain = 42' | ./zig-out/bin/repro-canonicalize --verbose
echo ""

echo "Testing with invalid inputs (should skip canonicalization):"
echo ""

# Test 4: Just an expression (no module header)
echo "Test 4: Expression without module header"
printf '42' | ./zig-out/bin/repro-canonicalize --verbose
echo ""

# Test 5: Invalid syntax
echo "Test 5: Invalid syntax"
printf 'module []\n\nx = = 5' | ./zig-out/bin/repro-canonicalize --verbose
echo ""

echo "Testing edge cases:"
echo ""

# Test 6: Empty input
echo "Test 6: Empty input"
printf '' | ./zig-out/bin/repro-canonicalize --verbose
echo ""

# Test 7: Module with undefined variable (should trigger canonicalization error)
echo "Test 7: Undefined variable"
printf 'module [x]\n\nx = y' | ./zig-out/bin/repro-canonicalize --verbose
echo ""

# Test 8: Random bytes (simulating fuzzer mutation)
echo "Test 8: Random bytes"
printf '\x00\x01\x02\x03' | ./zig-out/bin/repro-canonicalize --verbose
echo ""

echo "============================================="
echo "Fuzzer test complete!"
echo ""
echo "To run actual fuzzing with AFL++:"
echo "1. Install AFL++ (from source or package manager)"
echo "2. Build with: zig build -Dfuzz -Dsystem-afl=true"
echo "3. Run: ./zig-out/AFLplusplus/bin/afl-fuzz -i src/fuzz-corpus/canonicalize -o /tmp/canonicalize-out/ zig-out/bin/fuzz-canonicalize"
