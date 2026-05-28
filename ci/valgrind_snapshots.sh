#!/usr/bin/env bash
set -euo pipefail

snapshot_root="${SNAPSHOT_ROOT:-test/snapshots}"
snapshot_paths=()
while IFS= read -r -d '' snapshot_path; do
  snapshot_paths+=("$snapshot_path")
done < <(
  find "$snapshot_root" \
    \( -type d \( -name '*_package' -o -name '*_platform' -o -name '*_app' \) -print0 -prune \) -o \
    \( -type f -name '*.md' ! -name 'README.md' -print0 \)
)

if [ "${#snapshot_paths[@]}" -eq 0 ]; then
  echo "No snapshot paths found" >&2
  exit 1
fi

echo "Running ${#snapshot_paths[@]} snapshot paths under Valgrind"
./ci/custom_valgrind.sh "${SNAPSHOT_BIN:-./zig-out/bin/snapshot}" --debug --verbose "${snapshot_paths[@]}"
