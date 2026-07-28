#!/usr/bin/env bash
set -euo pipefail

if [[ "$(uname -s)" != "Linux" || "$(uname -m)" != "x86_64" ]]; then
    echo "integer SIMD codegen check is specific to Linux x86-64"
    exit 0
fi

if ! command -v objdump >/dev/null 2>&1; then
    echo "objdump is required for the integer SIMD codegen check" >&2
    exit 1
fi

roc_bin="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "$tmp_dir"' EXIT

cd "$repo_root"
"$roc_bin" build --opt=speed --no-cache --output="$tmp_dir/simd-smoke" test/cli/runtime_simd_smoke.roc >/dev/null
objdump -d --no-show-raw-insn "$tmp_dir/simd-smoke" >"$tmp_dir/disassembly"

for instruction in vpaddb vpmaddwd vpshufb vpclmullqlqdq; do
    if ! grep -Eq "[[:space:]]${instruction}[[:space:]]" "$tmp_dir/disassembly"; then
        echo "optimized integer SIMD smoke is missing ${instruction}" >&2
        exit 1
    fi
done

echo "integer SIMD codegen contains vpaddb, vpmaddwd, vpshufb, and vpclmullqlqdq"
