#!/usr/bin/env bash
set -euo pipefail

# FX File Execution Benchmarks
# Compares roc binary execution time between main and PR branches

MAIN_ROC=$1
PR_ROC=$2
BASE_BRANCH="${3:-}"  # Optional: base branch name
PR_BRANCH="${4:-}"    # Optional: PR/current branch name

# Check if file content differs between branches
# Returns 0 if same, 1 if different, 2 if comparison not possible
check_file_changed() {
    local file="$1"
    local base_branch="$2"
    local pr_branch="$3"

    if [ -z "$base_branch" ] || [ -z "$pr_branch" ]; then
        return 2  # Branches not provided
    fi

    local base_sha pr_sha
    base_sha=$(git show "$base_branch:$file" 2>/dev/null | shasum -a 256 | cut -d' ' -f1) || return 2
    pr_sha=$(git show "$pr_branch:$file" 2>/dev/null | shasum -a 256 | cut -d' ' -f1) || return 2

    if [ "$base_sha" != "$pr_sha" ]; then
        return 1  # Different
    fi
    return 0  # Same
}

# Print diff between branches for a file (indented for readability)
print_file_diff() {
    local file="$1"
    local base_branch="$2"
    local pr_branch="$3"

    echo "    Diff:"
    git diff "$base_branch".."$pr_branch" -- "$file" 2>/dev/null | sed 's/^/    /'
}

print_probe_log() {
    local label="$1"
    local probe_log="$2"

    echo "  $label:"
    if [ -s "$probe_log" ]; then
        tail -n 40 "$probe_log" | sed 's/^/    /'
    else
        echo "    <no output>"
    fi
}

exit_code_is_benchmarkable() {
    local exit_code="$1"
    local extra_args="$2"

    if [ "$exit_code" -eq 0 ]; then
        return 0
    fi

    if [[ "$extra_args" =~ --ignore-failure=([^[:space:]]+) ]]; then
        local ignore_codes="${BASH_REMATCH[1]}"
        local old_ifs="$IFS"
        IFS=','
        for code in $ignore_codes; do
            if [ "$exit_code" -eq "$code" ]; then
                IFS="$old_ifs"
                return 0
            fi
        done
        IFS="$old_ifs"
    fi

    if [[ "$extra_args" =~ --discard-failure=([^[:space:]]+) ]]; then
        local discard_codes="${BASH_REMATCH[1]}"
        local old_ifs="$IFS"
        IFS=','
        for code in $discard_codes; do
            if [ "$exit_code" -eq "$code" ]; then
                IFS="$old_ifs"
                return 0
            fi
        done
        IFS="$old_ifs"
    fi

    return 1
}

probe_command() {
    local roc_bin="$1"
    local fx_file="$2"
    local roc_extra_args="$3"
    local label="$4"
    local filename
    filename=$(basename "$fx_file")
    local probe_log="/tmp/bench_probe_${filename%.roc}_${label}.log"

    local -a cmd=("$roc_bin" "$fx_file" "--no-cache")
    if [ -n "$roc_extra_args" ]; then
        local -a extra_arg_array=()
        read -r -a extra_arg_array <<< "$roc_extra_args"
        cmd+=("${extra_arg_array[@]}")
    fi

    set +e
    "${cmd[@]}" >"$probe_log" 2>&1
    PROBE_EXIT_CODE=$?
    set -e
    PROBE_LOG="$probe_log"
}

preflight_benchmark() {
    local extra_args="$1"
    local main_roc="$2"
    local pr_roc="$3"
    local fx_file="$4"
    local roc_extra_args="$5"

    local filename
    filename=$(basename "$fx_file")

    probe_command "$main_roc" "$fx_file" "$roc_extra_args" "main"
    local main_exit_code="$PROBE_EXIT_CODE"
    local main_log="$PROBE_LOG"

    probe_command "$pr_roc" "$fx_file" "$roc_extra_args" "pr"
    local pr_exit_code="$PROBE_EXIT_CODE"
    local pr_log="$PROBE_LOG"

    if exit_code_is_benchmarkable "$main_exit_code" "$extra_args"; then
        if exit_code_is_benchmarkable "$pr_exit_code" "$extra_args"; then
            if cmp -s "$main_log" "$pr_log"; then
                return 0
            fi

            echo "Skipping $filename (main and PR outputs differ, so this is not a comparable benchmark)"
            echo "  main exit code: $main_exit_code"
            echo "  pr exit code: $pr_exit_code"
            print_probe_log "main output" "$main_log"
            print_probe_log "pr output" "$pr_log"
            PRECHECK_SKIP_REASON="output_diff"
            return 2
        fi

        echo "ERROR: PR command is not benchmarkable for $filename"
        echo "  main exit code: $main_exit_code"
        echo "  pr exit code: $pr_exit_code"
        print_probe_log "pr output" "$pr_log"
        return 1
    fi

    echo "Skipping $filename (main baseline is not benchmarkable here)"
    echo "  main exit code: $main_exit_code"
    echo "  pr exit code: $pr_exit_code"
    print_probe_log "main output" "$main_log"
    if ! exit_code_is_benchmarkable "$pr_exit_code" "$extra_args"; then
        print_probe_log "pr output" "$pr_log"
    fi
    PRECHECK_SKIP_REASON="main_unbenchmarkable"
    return 2
}

# Run hyperfine benchmark and return percentage change via global variable
# Returns 0 on success, 1 on failure
# Sets BENCH_PCT_CHANGE on success
run_benchmark() {
    local json_file="$1"
    local extra_args="$2"
    local main_roc="$3"
    local pr_roc="$4"
    local fx_file="$5"
    local roc_extra_args="$6"

    local cmd=(
        hyperfine
        --warmup 1
        --min-runs 3
        --shell=none
        --show-output
        --export-json "$json_file"
        $extra_args
        -n "main" "$main_roc $fx_file --no-cache $roc_extra_args"
        -n "pr" "$pr_roc $fx_file --no-cache $roc_extra_args"
    )

    echo "Running: ${cmd[*]}"

    if ! "${cmd[@]}" 2>&1; then
        return 1
    fi

    if [ -f "$json_file" ]; then
        local main_median pr_median
        main_median=$(jq -r '.results[] | select(.command | contains("main")) | .median' "$json_file")
        pr_median=$(jq -r '.results[] | select(.command | contains("pr")) | .median' "$json_file")

        if [ -n "$main_median" ] && [ -n "$pr_median" ] && [ "$main_median" != "null" ] && [ "$pr_median" != "null" ]; then
            BENCH_PCT_CHANGE=$(awk "BEGIN {printf \"%.2f\", (($pr_median - $main_median) / $main_median) * 100}")
            BENCH_ABS_DELTA_MS=$(awk "BEGIN {printf \"%.2f\", ($pr_median - $main_median) * 1000}")
            return 0
        fi
    fi
    return 1
}

echo "=== Building FX platform ==="
zig build test-platforms -Dplatform=fx -Doptimize=ReleaseFast

echo ""
echo "=== FX File Execution Benchmarks ==="
echo "Main binary: $MAIN_ROC"
echo "PR binary: $PR_ROC"
echo ""

# Collect all fx files, excluding those that use Stdin or don't have main! entry point
FX_FILES=""
for fx_file in test/fx/*.roc; do
    filename=$(basename "$fx_file")
    # Skip files that are flaky on CI
    if [ "$filename" = "dbg_corrupts_recursive_tag_union.roc" ]; then
        echo "Skipping $fx_file (flaky on CI)"
        continue
    fi
    # Skip files that don't have a main! entry point (app [ main! ])
    if ! grep -qE '^app[[:space:]]*\[[[:space:]]*main![[:space:]]*\]' "$fx_file" 2>/dev/null; then
        echo "Skipping $fx_file (no main! entry point)"
        continue
    fi
    # Skip files that import Stdin (they require interactive input)
    if grep -q "import pf.Stdin" "$fx_file" 2>/dev/null; then
        echo "Skipping $fx_file (uses Stdin)"
        continue
    fi
    FX_FILES="$FX_FILES $fx_file"
done

echo ""
echo "=== Running benchmarks ==="

SLOWER_DETECTED=0
SLOWER_FILES=""
CHANGED_FILES=""  # Files that differ between branches
SKIPPED_BASELINE_FILES=""
SKIPPED_OUTPUT_DIFF_FILES=""

for fx_file in $FX_FILES; do
    filename=$(basename "$fx_file")
    echo "--- Benchmarking: $filename ---"

    # Allow non-zero exit codes for files that are expected to fail
    # (compilation errors, runtime errors, or expected test failures)
    EXTRA_ARGS="--discard-failure=137"
    ROC_EXTRA_ARGS=""
    case "$filename" in
        division_by_zero.roc|\
        issue8433.roc|\
        test_type_mismatch.roc|\
        run_allow_errors.roc|\
        parse_error.roc|\
        run_warning_only.roc|\
        issue8517.roc|\
        stack_overflow_runtime.roc|\
        issue8826_full.roc|\
        issue8826_minimal.roc|\
        unused_state_var.roc|\
        issue8943.roc)
            EXTRA_ARGS+=" --ignore-failure=1,2"
            ;;
        num_method_call.roc)
            ROC_EXTRA_ARGS="--allow-errors"
            ;;
    esac

    if preflight_benchmark "$EXTRA_ARGS" "$MAIN_ROC" "$PR_ROC" "$fx_file" "$ROC_EXTRA_ARGS"; then
        :
    else
        preflight_status=$?
        if [ "$preflight_status" -eq 2 ]; then
            case "${PRECHECK_SKIP_REASON:-}" in
                output_diff)
                    SKIPPED_OUTPUT_DIFF_FILES="$SKIPPED_OUTPUT_DIFF_FILES $filename"
                    ;;
                *)
                    SKIPPED_BASELINE_FILES="$SKIPPED_BASELINE_FILES $filename"
                    ;;
            esac
            echo ""
            continue
        fi
        echo "ERROR: Preflight failed for $filename"
        exit 1
    fi

    # Run hyperfine comparison
    # Using median for robustness against outliers.
    # Note: Our "Change" percentage may differ slightly from hyperfine's "X times faster"
    # summary because hyperfine uses mean while we use median.
    if ! run_benchmark "/tmp/bench_${filename}.json" "$EXTRA_ARGS" "$MAIN_ROC" "$PR_ROC" "$fx_file" "$ROC_EXTRA_ARGS"; then
        echo "ERROR: Benchmark failed for $filename"
        exit 1
    fi

    pct_change="$BENCH_PCT_CHANGE"
    abs_delta_ms="$BENCH_ABS_DELTA_MS"
    echo "  Change: ${pct_change}% (${abs_delta_ms} ms)"

    # Check for meaningful slower execution - requires both relative and absolute slowdown.
    is_slower=$(awk "BEGIN {print ($pct_change > 4 && $abs_delta_ms > 5) ? 1 : 0}")
    if [ "$is_slower" = "1" ]; then
        echo "  Potential slowdown detected (${pct_change}%, ${abs_delta_ms} ms), running confirmation..."

        if ! run_benchmark "/tmp/bench_${filename}_confirm.json" "$EXTRA_ARGS" "$MAIN_ROC" "$PR_ROC" "$fx_file" "$ROC_EXTRA_ARGS"; then
            echo "ERROR: Confirmation benchmark failed for $filename"
            exit 1
        fi

        confirm_pct_change="$BENCH_PCT_CHANGE"
        confirm_abs_delta_ms="$BENCH_ABS_DELTA_MS"
        echo "  Confirmation change: ${confirm_pct_change}% (${confirm_abs_delta_ms} ms)"

        # Only report slowdown if both runs exceed both thresholds.
        confirm_is_slower=$(awk "BEGIN {print ($confirm_pct_change > 4 && $confirm_abs_delta_ms > 5) ? 1 : 0}")
        if [ "$confirm_is_slower" = "1" ]; then
            echo "  SLOWER EXECUTION CONFIRMED in $filename (${pct_change}% / ${abs_delta_ms} ms then ${confirm_pct_change}% / ${confirm_abs_delta_ms} ms)"
            SLOWER_DETECTED=1
            SLOWER_FILES="$SLOWER_FILES $filename"

            # Check if file content changed between branches
            check_file_changed "$fx_file" "$BASE_BRANCH" "$PR_BRANCH" || check_result=$?
            if [ "${check_result:-0}" = "1" ]; then
                echo "    NOTE: File content differs between branches - comparison may not be meaningful"
                print_file_diff "$fx_file" "$BASE_BRANCH" "$PR_BRANCH"
                CHANGED_FILES="$CHANGED_FILES $filename"
            fi
            unset check_result
        else
            echo "  Slowdown NOT confirmed (first: ${pct_change}% / ${abs_delta_ms} ms, second: ${confirm_pct_change}% / ${confirm_abs_delta_ms} ms)"
        fi
    fi
    echo ""
done

# Summary
echo "=== FX Benchmark Summary ==="
if [ -n "$SKIPPED_BASELINE_FILES" ]; then
    echo "Skipped benchmarks because the main baseline command was not benchmarkable:"
    for f in $SKIPPED_BASELINE_FILES; do
        echo "  - $f"
    done
    echo ""
fi
if [ -n "$SKIPPED_OUTPUT_DIFF_FILES" ]; then
    echo "Skipped benchmarks because the main and PR outputs differed:"
    for f in $SKIPPED_OUTPUT_DIFF_FILES; do
        echo "  - $f"
    done
    echo ""
fi
if [ "$SLOWER_DETECTED" = "1" ]; then
    echo "SLOWER EXECUTION detected in the following files:"
    for f in $SLOWER_FILES; do
        # Check if this file is in the CHANGED_FILES list
        if echo "$CHANGED_FILES" | grep -qw "$f"; then
            echo "  - $f (file changed)"
        else
            echo "  - $f"
        fi
    done
    echo ""
    echo "Ask Richard, Anton, or Luke to override this failure if you believe it is justified."
    exit 1
else
    echo "No significant slowdowns detected (threshold: >4% and >5 ms)"
fi
