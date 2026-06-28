#!/usr/bin/env bash
set -euo pipefail

# Benchmark corpus verification.
#
# The comparison benchmark is allowed to skip a file when the main-branch
# baseline is not benchmarkable. This PR-only verification runs the benchmark
# command set against one Roc binary and fails on PR segfaults, so benchmark
# skips cannot hide new crashes in the branch being tested.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

ROC_BIN="${1:-}"

usage() {
    cat <<EOF
Usage: $(basename "$0") <roc-binary>

Verify every FX benchmark command once with <roc-binary> and fail if the
binary segfaults.
EOF
}

error() {
    echo "ERROR: $1" >&2
    exit 1
}

if [ -z "$ROC_BIN" ]; then
    usage
    exit 2
fi

cd "$REPO_ROOT"

if [ ! -x "$ROC_BIN" ]; then
    error "Roc binary is not executable: $ROC_BIN"
fi

is_intentional_error_fixture() {
    local filename="$1"

    # These files intentionally exercise diagnostics, --allow-errors, or
    # runtime crash reporting. They are test fixtures, not successful execution
    # benchmarks. Do not add compiler crash/regression repros here; those should
    # fail if the PR binary cannot run them.
    case "$filename" in
        allow_errors_type_mismatch.roc|\
        division_by_zero.roc|\
        inspect_wrong_sig_test.roc|\
        issue8433.roc|\
        issue8517.roc|\
        issue8826_full.roc|\
        issue8826_minimal.roc|\
        issue8943.roc|\
        num_method_call.roc|\
        parse_error.roc|\
        run_allow_errors.roc|\
        stack_overflow_runtime.roc|\
        test_type_mismatch.roc|\
        unused_state_var.roc)
            return 0
            ;;
    esac

    return 1
}

print_probe_log() {
    local probe_log="$1"

    if [ -s "$probe_log" ]; then
        tail -n 40 "$probe_log" | sed 's/^/    /'
    else
        echo "    <no output>"
    fi
}

print_lldb_backtrace() {
    local -a cmd=("$@")

    if ! command -v lldb >/dev/null 2>&1; then
        return 0
    fi

    echo "  lldb backtrace:"
    lldb --batch \
        -o "run" \
        -o "thread backtrace all" \
        -- "${cmd[@]}" 2>&1 | tail -n 200 | sed 's/^/    /' || true
}

run_probe() {
    local fx_file="$1"
    local roc_subcommand="$2"

    local filename
    filename=$(basename "$fx_file")

    local display_name="$filename"
    local subcommand_suffix=""
    if [ -n "$roc_subcommand" ]; then
        display_name="$roc_subcommand $filename"
        subcommand_suffix="_$roc_subcommand"
    fi

    local probe_log="/tmp/benchmark_verify_${filename%.roc}${subcommand_suffix}.log"

    local -a cmd=("$ROC_BIN")
    if [ -n "$roc_subcommand" ]; then
        cmd+=("$roc_subcommand")
    fi
    cmd+=("$fx_file" "--no-cache")

    echo "--- Verify: $display_name ---"

    set +e
    "${cmd[@]}" >"$probe_log" 2>&1
    local exit_code=$?
    set -e

    if [ "$exit_code" -eq 139 ] || grep -qi "Segmentation fault" "$probe_log"; then
        echo "ERROR: $display_name printed a segmentation fault"
        echo "  exit code: $exit_code"
        print_probe_log "$probe_log"
        print_lldb_backtrace "${cmd[@]}"
        return 1
    fi

    return 0
}

echo "=== Building FX platform ==="
zig build build-test-hosts -Dplatform=fx -Doptimize=ReleaseFast

echo ""
echo "=== Collecting FX benchmark files ==="

FX_FILES=()
for fx_file in test/fx/*.roc; do
    filename=$(basename "$fx_file")
    if is_intentional_error_fixture "$filename"; then
        echo "Skipping $fx_file (intentional error fixture)"
        continue
    fi
    if ! grep -qE '^app[[:space:]]*\[[[:space:]]*main![[:space:]]*\]' "$fx_file" 2>/dev/null; then
        echo "Skipping $fx_file (no main! entry point)"
        continue
    fi
    if grep -q "import pf.Stdin" "$fx_file" 2>/dev/null; then
        echo "Skipping $fx_file (uses Stdin)"
        continue
    fi
    FX_FILES+=("$fx_file")
done

CHECK_BUILD_FILES=(
    test/fx/issue8826_full.roc
    test/fx/aoc_day2.roc
    test/fx/host_boxed_fn_boundary.roc
    test/fx/record_builder_cli_parser.roc
    test/fx/zst_nested_singleton_shapes.roc
    test/fx/index_oob_instantiate.roc
    test/fx/wildcard_match_open_union_bug.roc
    test/fx/repeating_pattern_segfault.roc
    test/fx/primitive_encode.roc
    test/fx/dbg_corrupts_recursive_tag_union.roc
)

echo ""
echo "=== Verifying benchmark commands ==="

VERIFY_FAILED=0

for fx_file in "${FX_FILES[@]}"; do
    if ! run_probe "$fx_file" ""; then
        VERIFY_FAILED=1
    fi
done

echo ""
echo "=== Verifying roc check commands ==="
for fx_file in "${CHECK_BUILD_FILES[@]}"; do
    if ! run_probe "$fx_file" "check"; then
        VERIFY_FAILED=1
    fi
done

echo ""
echo "=== Verifying roc build commands ==="
for fx_file in "${CHECK_BUILD_FILES[@]}"; do
    if ! run_probe "$fx_file" "build"; then
        VERIFY_FAILED=1
    fi
done

echo ""
if [ "$VERIFY_FAILED" = "1" ]; then
    echo "Benchmark verification failed"
    exit 1
fi

echo "Benchmark verification passed"
