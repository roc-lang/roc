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
# So this counts the instructions in the generated Roc entrypoint and fails if
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

roc_bin="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
fixture="$repo_root/test/cli/match_extension_codegen.roc"
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "$tmp_dir"' EXIT

cd "$repo_root"

# target:expected-instruction-count
# arm64musl dropped from 91 to 82 when the aarch64 CPU model moved from
# cortex_a76 to generic plus the named AES and DotProd features, so that
# arm64musl binaries run on Armv8.0-A hardware. The whole count is one
# procedure, and the loop it pins is unchanged; what went away is scheduling
# driven by the cortex_a76 model, which unrolled this loop one step further.
# Fewer instructions here means less unrolling, not less work per byte.
# x64musl rose from 95 to 99 when the x86-64-v3 model was constrained to the
# instruction features declared by Roc's CPU contract. This prevents LLVM's
# named model from silently raising the runtime instruction floor.
# Both counts rose (x64musl 99 to 124, arm64musl 82 to 92) when generated
# procs and linked builtins gained inline hints: the surrounding list setup
# inlines into the procedure instead of staying behind calls. The pinned
# eight-byte compare loop is unchanged - load, load, compare, advance, with
# the from_le_bytes bounds test still doubling as the loop's termination.
# Evaluating the later position first makes its bounds check govern both reads.
# This keeps the fast loop to one bound branch per eight bytes when proven
# no-wrap arithmetic gives LLVM stronger induction-variable facts.
# Exact single-use inlining before ARC adds the release of the owned root
# argument list after its length is read. Restoring internal linkage to
# procedures not named by static-data relocations then lets LLVM optimize that
# body together with its exported roc_main wrapper. These transformations
# interact rather than adding their separate instruction-count changes: the
# resulting entrypoint totals are 106 on x64musl and 92 on arm64musl. The
# pinned compare loop itself is unchanged.
expectations=(
    "x64musl:106"
    "arm64musl:92"
)

failed=0

count_objdump_instructions() {
    objdump -d --no-show-raw-insn "$1" | awk '
        /^[0-9a-f]+ <_?roc_main>/ { in_proc = 1; found = 1; next }
        /^[0-9a-f]+ </           { in_proc = 0 }
        in_proc && /^[[:space:]]+[0-9a-f]+:/ { count++ }
        END {
            if (!found) exit 1
            print count + 0
        }
    '
}

count_aarch64_instructions() {
    local size_hex
    local size_bytes

    size_hex="$(objdump -t "$1" | awk '
        $NF ~ /^_?roc_main$/ {
            matches++
            size = $(NF - 1)
        }
        END {
            if (matches != 1) exit 1
            print size
        }
    ')"
    size_hex="${size_hex#0x}"
    if [[ ! "$size_hex" =~ ^[0-9a-fA-F]+$ ]]; then
        return 1
    fi

    size_bytes=$((16#$size_hex))
    if ((size_bytes % 4 != 0)); then
        return 1
    fi

    printf '%d\n' "$((size_bytes / 4))"
}

for entry in "${expectations[@]}"; do
    target="${entry%%:*}"
    expected="${entry##*:}"

    "$roc_bin" build --opt=speed --no-cache --target="$target" \
        --output="$tmp_dir/match-$target" "$fixture" >/dev/null

    case "$target" in
        arm64musl)
            # Every AArch64 instruction is four bytes. Counting the exact
            # entrypoint symbol size keeps this measurement independent of
            # host-specific disassembly formatting.
            actual="$(count_aarch64_instructions "$tmp_dir/match-$target")"
            ;;
        x64musl)
            actual="$(count_objdump_instructions "$tmp_dir/match-$target")"
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

To see what changed, build the fixture and disassemble the roc_main symbol:

    roc build --opt=speed --no-cache --target=<target> \
        --output=/tmp/match test/cli/match_extension_codegen.roc
    objdump -d --no-show-raw-insn /tmp/match

For arm64musl, `objdump -t /tmp/match` reports the entrypoint byte size;
AArch64 instructions are four bytes each.

For why a given instruction is there, set `dump_llvm_artifacts` to true in
src/cli/builder.zig to also get the optimized LLVM IR: that distinguishes a
decision made in the middle end from one made during instruction selection,
which the final disassembly alone cannot.
EOF
    exit 1
fi
