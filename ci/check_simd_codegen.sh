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

if ! command -v python3 >/dev/null 2>&1; then
    echo "python3 is required for the integer SIMD codegen check" >&2
    exit 1
fi

roc_bin="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "$tmp_dir"' EXIT

cd "$repo_root"
"$roc_bin" build --opt=speed --no-cache --output="$tmp_dir/simd-smoke-speed" test/cli/runtime_simd_smoke.roc >/dev/null
objdump -d --no-show-raw-insn "$tmp_dir/simd-smoke-speed" >"$tmp_dir/speed-disassembly"

for instruction in vpaddb vpmaddwd vpshufb vpclmullqlqdq; do
    if ! grep -Eq "[[:space:]]${instruction}[[:space:]]" "$tmp_dir/speed-disassembly"; then
        echo "optimized integer SIMD smoke is missing ${instruction}" >&2
        exit 1
    fi
done

# Keep the complete Teddy chain: a bare comparison can hide scalarization.
"$roc_bin" build --opt=speed --no-cache --target=arm64musl --output="$tmp_dir/prefilter-aarch64" test/cli/simd_prefilter_codegen.roc >/dev/null
"$roc_bin" build --opt=speed --no-cache --target=x64musl --output="$tmp_dir/prefilter-x86" test/cli/simd_prefilter_codegen.roc >/dev/null
objdump -d --no-show-raw-insn --disassemble=roc_main "$tmp_dir/prefilter-x86" >"$tmp_dir/prefilter-x86-disassembly"

count_prefilter() {
    python3 ci/count_aarch64_instructions.py "$tmp_dir/prefilter-aarch64" roc_main "$1"
}

if [[ "$(count_prefilter tbl)" -lt 6 ]]; then
    echo "optimized AArch64 prefilter is missing its six table lookups" >&2
    exit 1
fi
if ! grep -Eq "[[:space:]]v?palignr[[:space:]]" "$tmp_dir/prefilter-x86-disassembly"; then
    echo "optimized x86-64 prefilter is missing palignr" >&2
    exit 1
fi
for instruction in umov-byte extr; do
    count="$(count_prefilter "$instruction")"
    if [[ "$count" != "0" ]]; then
        echo "optimized AArch64 prefilter contains $count scalar ${instruction} instructions" >&2
        exit 1
    fi
done
if [[ "$(count_prefilter ext)" -lt 2 ]]; then
    echo "optimized AArch64 prefilter is missing its vector byte aligns" >&2
    exit 1
fi
if ! grep -Eq "[[:space:]]v?pmovmskb[[:space:]]" "$tmp_dir/prefilter-x86-disassembly"; then
    echo "optimized x86-64 prefilter is missing pmovmskb" >&2
    exit 1
fi

"$roc_bin" build --opt=dev --no-cache --output="$tmp_dir/simd-smoke-dev" test/cli/runtime_simd_smoke.roc >/dev/null
objdump -d --no-show-raw-insn "$tmp_dir/simd-smoke-dev" >"$tmp_dir/dev-disassembly"

if grep -Eq "[[:space:]]call[[:space:]].*<roc_builtins_simd_eval>" "$tmp_dir/dev-disassembly"; then
    echo "dev integer SIMD smoke calls scalar roc_builtins_simd_eval" >&2
    exit 1
fi

for instruction in paddb pmaddwd pshufb pclmullqlqdq; do
    if ! grep -Eq "[[:space:]]v?${instruction}[[:space:]]" "$tmp_dir/dev-disassembly"; then
        echo "dev integer SIMD smoke is missing native ${instruction}" >&2
        exit 1
    fi
done

for forbidden_symbol in roc_builtins_simd_eval roc_builtins_simd_load_16; do
    if strings "$tmp_dir/simd-smoke-dev" | grep -Fq "$forbidden_symbol"; then
        echo "dev x86-64 integer SIMD binary still links deleted helper ${forbidden_symbol}" >&2
        exit 1
    fi
done

# Instantiate the exhaustive source corpus through the other dev backend too.
# This exercises every supported low-level/type pairing, not merely the four
# smoke-test instruction classes above. The target binary is not executable on
# this runner, so semantic execution remains covered by the shared differential
# corpus on native builders and the byte-exact AArch64 emitter unit tests.
"$roc_bin" build --opt=dev --target=arm64musl --no-cache --output="$tmp_dir/simd-differential-aarch64" test/simd/differential.roc >/dev/null
for forbidden_symbol in roc_builtins_simd_eval roc_builtins_simd_load_16; do
    if strings "$tmp_dir/simd-differential-aarch64" | grep -Fq "$forbidden_symbol"; then
        echo "dev AArch64 integer SIMD binary still links deleted helper ${forbidden_symbol}" >&2
        exit 1
    fi
done

echo "optimized x86-64 and AArch64 and both dev backends contain native packed SIMD lowering"
