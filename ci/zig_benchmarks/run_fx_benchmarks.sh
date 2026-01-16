#!/usr/bin/env bash
set -euo pipefail

# FX File Execution Benchmarks
# Compares roc binary execution time between main and PR branches

MAIN_ROC=$1
PR_ROC=$2

echo "=== FX File Execution Benchmarks ==="
echo "Main binary: $MAIN_ROC"
echo "PR binary: $PR_ROC"
echo ""

# Collect all fx files, excluding those that use Stdin
FX_FILES=""
for fx_file in test/fx/*.roc; do
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

for fx_file in $FX_FILES; do
    filename=$(basename "$fx_file")
    echo "--- Benchmarking: $filename ---"

    # Run hyperfine comparison
    # --warmup 1: 1 warmup run before timing
    # --min-runs 3: at least 3 timed runs
    # --shell=none: direct execution without shell
    # --export-json: export for parsing
    if ! hyperfine \
        --warmup 1 \
        --min-runs 3 \
        --shell=none \
        --export-json "/tmp/bench_${filename}.json" \
        -n "main" "$MAIN_ROC $fx_file" \
        -n "pr" "$PR_ROC $fx_file" \
        2>&1; then
        echo "  Warning: Benchmark failed for $filename, skipping"
        continue
    fi

    # Parse JSON to detect slower execution (PR slower than main by >5%)
    # Using median for robustness against outliers
    if [ -f "/tmp/bench_${filename}.json" ]; then
        main_median=$(jq -r '.results[] | select(.command | contains("main")) | .median' "/tmp/bench_${filename}.json")
        pr_median=$(jq -r '.results[] | select(.command | contains("pr")) | .median' "/tmp/bench_${filename}.json")

        # Calculate percentage change
        if [ -n "$main_median" ] && [ -n "$pr_median" ] && [ "$main_median" != "null" ] && [ "$pr_median" != "null" ]; then
            pct_change=$(echo "scale=2; (($pr_median - $main_median) / $main_median) * 100" | bc)
            echo "  Change: ${pct_change}%"

            # Check for >5% slower execution
            is_slower=$(echo "$pct_change > 5" | bc)
            if [ "$is_slower" = "1" ]; then
                echo "  SLOWER EXECUTION in $filename (${pct_change}% slower)"
                SLOWER_DETECTED=1
                SLOWER_FILES="$SLOWER_FILES $filename"
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
        echo "  - $f"
    done
    echo ""
    echo "Ask Richard, Anton, or Luke to override this failure if you believe it is justified."
    exit 1
else
    echo "No significant slowdowns detected (threshold: >5%)"
fi
