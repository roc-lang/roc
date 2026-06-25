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

snapshot_bin="${SNAPSHOT_BIN:-}"
if [ -z "$snapshot_bin" ]; then
  valgrind_shared_memory_size="${VALGRIND_SNAPSHOT_SHARED_MEMORY_SIZE:-268435456}"
  echo "Building snapshot tool for Valgrind with shared-memory arena size ${valgrind_shared_memory_size}"
  zig build \
    -Doptimize="${VALGRIND_SNAPSHOT_OPTIMIZE:-ReleaseFast}" \
    -Dstrip=false \
    -Dshared-memory-size="${valgrind_shared_memory_size}" \
    build-snapshot-tool
  snapshot_bin="./zig-out/bin/snapshot"
fi

if [ ! -x "$snapshot_bin" ]; then
  echo "Snapshot binary not found or not executable: $snapshot_bin" >&2
  exit 1
fi

export TMPDIR="${TMPDIR:-$PWD/zig-out/tmp/valgrind-snapshots}"
mkdir -p "$TMPDIR"

echo "Running ${#snapshot_paths[@]} snapshot paths under Valgrind"
./ci/custom_valgrind.sh "$snapshot_bin" --debug --verbose "${snapshot_paths[@]}"
