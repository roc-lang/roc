#!/usr/bin/env bash

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Zig/LLVM stack probes read one word at SP-4096 before moving SP.
valgrind \
  --suppressions="$SCRIPT_DIR/valgrind.supp" \
  --ignore-range-below-sp=4096-4093 \
  --leak-check=full \
  --error-exitcode=1 \
  --errors-for-leak-kinds=definite,possible \
  "$@" 2>&1 | grep -v "Warning: DWARF2 reader: Badly formed extended line op encountered"
exit ${PIPESTATUS[0]}