#!/usr/bin/env bash

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

valgrind \
  --suppressions="$SCRIPT_DIR/valgrind.supp" \
  --leak-check=full \
  --error-exitcode=0 \
  --errors-for-leak-kinds=definite,possible \
  --track-origins=yes \
  --gen-suppressions=all \
  "$@" 2>&1 | grep -v "Warning: DWARF2 reader: Badly formed extended line op encountered"
exit ${PIPESTATUS[0]}
