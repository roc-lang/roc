#!/usr/bin/env bash
set -euo pipefail

# Snapshot Tool Execution Benchmarks
# Compares snapshot tool execution time between main and PR branches

MAIN_SNAPSHOT=$1
PR_SNAPSHOT=$2
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
    CHANGED_FILES=""  # Files that differ between branches

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

                    # Check if file content changed between branches
                    check_file_changed "$snapshot_file" "$BASE_BRANCH" "$PR_BRANCH" || check_result=$?
                    if [ "${check_result:-0}" = "1" ]; then
                        echo "    NOTE: File content differs between branches - comparison may not be meaningful"
                        print_file_diff "$snapshot_file" "$BASE_BRANCH" "$PR_BRANCH"
                        CHANGED_FILES="$CHANGED_FILES $filename"
                    fi
                    unset check_result
                fi
            fi
        fi
    done

    echo ""
    if [ -n "$SLOWEST_FILES" ]; then
        echo "Snapshots with >5% slower execution:"
        for f in $SLOWEST_FILES; do
            # Check if this file is in the CHANGED_FILES list
            if echo "$CHANGED_FILES" | grep -qw "$f"; then
                echo "  - $f (file changed)"
            else
                echo "  - $f"
            fi
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
