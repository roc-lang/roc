#!/usr/bin/env bash
set -euo pipefail

# Snapshot Tool Execution Benchmarks
# Compares snapshot tool execution time between main and PR branches

MAIN_SNAPSHOT=$1
PR_SNAPSHOT=$2

echo "=== Snapshot Tool Execution Benchmarks ==="
echo "Main snapshot: $MAIN_SNAPSHOT"
echo "PR snapshot: $PR_SNAPSHOT"
echo ""

# Run overall snapshot benchmark
echo "=== Running overall snapshot benchmark ==="

hyperfine \
    --warmup 1 \
    --min-runs 3 \
    --shell=none \
    --export-json "/tmp/snapshot_bench.json" \
    -n "main" "$MAIN_SNAPSHOT" \
    -n "pr" "$PR_SNAPSHOT"

# Parse JSON to detect slower execution (PR slower than main by >5%)
# Using median for robustness against outliers.
# Note: Our "Change" percentage may differ slightly from hyperfine's "X times faster"
# summary because hyperfine uses mean while we use median.
main_median=$(jq -r '.results[] | select(.command | contains("main")) | .median' "/tmp/snapshot_bench.json")
pr_median=$(jq -r '.results[] | select(.command | contains("pr")) | .median' "/tmp/snapshot_bench.json")

echo ""
echo "Main median: ${main_median}s"
echo "PR median: ${pr_median}s"

# Calculate percentage change (using awk for floating point precision)
pct_change=$(awk "BEGIN {printf \"%.2f\", (($pr_median - $main_median) / $main_median) * 100}")
echo "Change: ${pct_change}%"

# Check for >5% slower execution
is_slower=$(awk "BEGIN {print ($pct_change > 5) ? 1 : 0}")
if [ "$is_slower" = "1" ]; then
    echo ""
    echo "SLOWER EXECUTION detected (${pct_change}% slower)"
    echo ""
    echo "=== Running granular snapshot benchmarks to identify cause ==="

    # Run benchmarks on individual snapshot files to find the cause
    SLOWEST_FILES=""

    for snapshot_file in test/snapshots/*.md; do
        filename=$(basename "$snapshot_file")

        # Skip README
        if [ "$filename" = "README.md" ]; then
            continue
        fi

        # Quick benchmark of individual file
        if hyperfine \
            --warmup 1 \
            --min-runs 3 \
            --shell=none \
            --export-json "/tmp/snap_${filename}.json" \
            -n "main" "$MAIN_SNAPSHOT $snapshot_file" \
            -n "pr" "$PR_SNAPSHOT $snapshot_file" \
            2>/dev/null; then

            file_main=$(jq -r '.results[] | select(.command | contains("main")) | .median' "/tmp/snap_${filename}.json")
            file_pr=$(jq -r '.results[] | select(.command | contains("pr")) | .median' "/tmp/snap_${filename}.json")

            if [ -n "$file_main" ] && [ -n "$file_pr" ] && [ "$file_main" != "null" ] && [ "$file_pr" != "null" ]; then
                file_pct=$(awk "BEGIN {printf \"%.2f\", (($file_pr - $file_main) / $file_main) * 100}")
                file_is_slower=$(awk "BEGIN {print ($file_pct > 5) ? 1 : 0}")
                if [ "$file_is_slower" = "1" ]; then
                    echo "  $filename: ${file_pct}% slower"
                    SLOWEST_FILES="$SLOWEST_FILES $filename"
                fi
            fi
        fi
    done

    echo ""
    if [ -n "$SLOWEST_FILES" ]; then
        echo "Snapshots with >5% slower execution:"
        for f in $SLOWEST_FILES; do
            echo "  - $f"
        done
    else
        echo "No individual snapshots showed >5% slowdown (may be distributed across many files)"
    fi

    echo ""
    echo "Ask Richard, Anton, or Luke to override this failure if you believe it is justified."
    exit 1
else
    echo ""
    echo "No significant slowdown detected (threshold: >5%)"
fi
