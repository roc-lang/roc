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

echo "=== Building FX platform ==="
zig build test-platforms -Dplatform=fx

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

    # Run hyperfine comparison
    if ! hyperfine \
        --warmup 1 \
        --min-runs 3 \
        --shell=none \
        --show-output \
        --export-json "/tmp/bench_${filename}.json" \
        $EXTRA_ARGS \
        -n "main" "$MAIN_ROC $fx_file --no-cache $ROC_EXTRA_ARGS" \
        -n "pr" "$PR_ROC $fx_file --no-cache $ROC_EXTRA_ARGS" \
        2>&1; then
        echo "ERROR: Benchmark failed for $filename"
        exit 1
    fi

    # Parse JSON to detect slower execution (PR slower than main by >5%)
    # Using median for robustness against outliers.
    # Note: Our "Change" percentage may differ slightly from hyperfine's "X times faster"
    # summary because hyperfine uses mean while we use median.
    if [ -f "/tmp/bench_${filename}.json" ]; then
        main_median=$(jq -r '.results[] | select(.command | contains("main")) | .median' "/tmp/bench_${filename}.json")
        pr_median=$(jq -r '.results[] | select(.command | contains("pr")) | .median' "/tmp/bench_${filename}.json")

        # Calculate percentage change (using awk for floating point precision)
        if [ -n "$main_median" ] && [ -n "$pr_median" ] && [ "$main_median" != "null" ] && [ "$pr_median" != "null" ]; then
            pct_change=$(awk "BEGIN {printf \"%.2f\", (($pr_median - $main_median) / $main_median) * 100}")
            echo "  Change: ${pct_change}%"

            # Check for >5% slower execution
            is_slower=$(awk "BEGIN {print ($pct_change > 5) ? 1 : 0}")
            if [ "$is_slower" = "1" ]; then
                echo "  SLOWER EXECUTION in $filename (${pct_change}% slower)"
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
            fi
        fi
    fi
    echo ""
done

# Summary
echo "=== FX Benchmark Summary ==="
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
    echo "No significant slowdowns detected (threshold: >5%)"
fi
