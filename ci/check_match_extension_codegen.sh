#!/usr/bin/env bash
# Pin the code generated for an LZ77-style match-extension loop.
#
# test/cli/match_extension_codegen.roc is the inner loop compressors spend most
# of their time in: compare two positions in a byte list eight bytes at a time.
# Getting it from ~25 instructions per iteration down to ~11 took a specific
# combination of a bounds check the optimizer can hoist, a loop whose only exit
# is that check, and wrapping index arithmetic. Each of those is easy to undo by
# accident while changing something that looks unrelated, and nothing else in
# the test suite would notice.
#
# So this counts the instructions in the generated Roc procedures and fails if
# the number moves in either direction. A drop is as interesting as a rise: it
# usually means the fixture stopped measuring what it was written to measure.
#
# Counts are per target, because they legitimately differ, and every target is
# pinned so a regression on one cannot hide behind another. Targets are named
# explicitly rather than built natively, so the numbers do not depend on which
# machine runs the check.
set -euo pipefail

if ! command -v objdump >/dev/null 2>&1; then
    echo "objdump is required for the match extension codegen check" >&2
    exit 1
fi
if ! command -v readelf >/dev/null 2>&1; then
    echo "readelf is required for the match extension codegen check" >&2
    exit 1
fi

roc_bin="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
fixture="$repo_root/test/cli/match_extension_codegen.roc"
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "$tmp_dir"' EXIT

cd "$repo_root"

# target:expected-instruction-count
expectations=(
    "x64musl:95"
    "arm64musl:91"
)

failed=0
for entry in "${expectations[@]}"; do
    target="${entry%%:*}"
    expected="${entry##*:}"

    "$roc_bin" build --opt=speed --no-cache --target="$target" \
        --output="$tmp_dir/match-$target" "$fixture" >/dev/null

    case "$target" in
        arm64musl)
            # Every AArch64 instruction is exactly four bytes. Reading the
            # exact procedure symbol sizes avoids depending on a host objdump
            # configured with the AArch64 disassembler.
            actual="$(readelf -sW "$tmp_dir/match-$target" | awk '
                $8 ~ /^_?roc__proc/ { bytes += $3 }
                END {
                    if (bytes % 4 != 0) exit 1
                    print bytes / 4
                }
            ')"
            ;;
        x64musl)
            actual="$(objdump -d --no-show-raw-insn "$tmp_dir/match-$target" | awk '
                /^[0-9a-f]+ <_?roc__proc/ { in_proc = 1; next }
                /^[0-9a-f]+ </           { in_proc = 0 }
                in_proc && /^[[:space:]]+[0-9a-f]+:/ { count++ }
                END { print count + 0 }
            ')"
            ;;
        *)
            echo "match extension codegen has no instruction counter for $target" >&2
            exit 1
            ;;
    esac

    if [ "$actual" = "$expected" ]; then
        echo "match extension codegen: $target has $actual instructions"
    else
        failed=1
        echo "match extension codegen changed on $target: expected $expected instructions, got $actual" >&2
    fi
done

if [ "$failed" -ne 0 ]; then
    cat >&2 <<'EOF'

Code generation for the match-extension loop changed. This is not automatically
a bug, but it is worth understanding before updating the numbers above.

To see what changed, build the fixture and disassemble the roc__proc symbols:

    roc build --opt=speed --no-cache --target=<target> \
        --output=/tmp/match test/cli/match_extension_codegen.roc
    objdump -d --no-show-raw-insn /tmp/match

For arm64musl, `readelf -sW /tmp/match` reports procedure byte sizes;
AArch64 instructions are four bytes each.

For why a given instruction is there, set `dump_llvm_artifacts` to true in
src/cli/builder.zig to also get the optimized LLVM IR: that distinguishes a
decision made in the middle end from one made during instruction selection,
which the final disassembly alone cannot.
EOF
    exit 1
fi
