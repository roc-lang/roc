#!/usr/bin/env bash
set -euo pipefail

# Local Benchmark Runner
# Compares current branch against main (or specified base) branch
# Replicates the CI benchmark workflow for local testing

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Default options
BASE_BRANCH="main"
SKIP_FX=false
SKIP_SNAPSHOT=false
KEEP_ARTIFACTS=false

# State tracking for cleanup
ORIGINAL_BRANCH=""
STASH_CREATED=false
CLEANUP_DONE=false
TEMP_SCRIPTS_DIR=""

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Run zig compiler benchmarks locally, comparing current branch against base branch.

OPTIONS:
    --base <branch>     Base branch to compare against (default: main)
    --skip-fx           Skip FX file benchmarks
    --skip-snapshot     Skip snapshot tool benchmarks
    --keep-artifacts    Keep bench-main and bench-local directories after run
    -h, --help          Show this help message

PREREQUISITES:
    - hyperfine (benchmark tool)
    - jq (JSON processor)
    - zig (0.15.2)

EXAMPLES:
    $(basename "$0")                    # Compare current branch vs main
    $(basename "$0") --base develop     # Compare current branch vs develop
    $(basename "$0") --skip-snapshot    # Only run FX benchmarks
EOF
}

log() {
    echo "=== $1 ==="
}

error() {
    echo "ERROR: $1" >&2
    exit 1
}

check_prerequisites() {
    log "Checking prerequisites"

    if ! command -v hyperfine &>/dev/null; then
        error "hyperfine is not installed. Install with: brew install hyperfine"
    fi
    echo "  hyperfine: $(hyperfine --version)"

    if ! command -v jq &>/dev/null; then
        error "jq is not installed. Install with: brew install jq"
    fi
    echo "  jq: $(jq --version)"

    if ! command -v zig &>/dev/null; then
        error "zig is not installed"
    fi
    echo "  zig: $(zig version)"
    echo ""
}

cleanup() {
    # Prevent double cleanup
    if [ "$CLEANUP_DONE" = "true" ]; then
        return
    fi
    CLEANUP_DONE=true

    echo ""
    log "Cleaning up"

    # Restore original branch
    if [ -n "$ORIGINAL_BRANCH" ]; then
        echo "  Restoring branch: $ORIGINAL_BRANCH"
        git checkout "$ORIGINAL_BRANCH" --quiet 2>/dev/null || true
    fi

    # Restore stashed changes
    if [ "$STASH_CREATED" = "true" ]; then
        echo "  Restoring stashed changes"
        git stash pop --quiet 2>/dev/null || true
    fi

    # Clean up temp scripts
    if [ -n "$TEMP_SCRIPTS_DIR" ] && [ -d "$TEMP_SCRIPTS_DIR" ]; then
        echo "  Removing temporary scripts"
        rm -rf "$TEMP_SCRIPTS_DIR"
    fi

    # Clean up artifacts unless requested to keep them
    if [ "$KEEP_ARTIFACTS" = "false" ]; then
        echo "  Removing benchmark artifacts"
        rm -rf "$REPO_ROOT/bench-main" "$REPO_ROOT/bench-local"
    else
        echo "  Keeping benchmark artifacts in bench-main/ and bench-local/"
    fi

    echo "  Done"
}

# Set up cleanup trap
trap cleanup EXIT

parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --base)
                BASE_BRANCH="$2"
                shift 2
                ;;
            --skip-fx)
                SKIP_FX=true
                shift
                ;;
            --skip-snapshot)
                SKIP_SNAPSHOT=true
                shift
                ;;
            --keep-artifacts)
                KEEP_ARTIFACTS=true
                shift
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
    done

    if [ "$SKIP_FX" = "true" ] && [ "$SKIP_SNAPSHOT" = "true" ]; then
        error "Cannot skip both FX and snapshot benchmarks"
    fi
}

main() {
    parse_args "$@"

    cd "$REPO_ROOT"

    check_prerequisites

    # Record current branch
    ORIGINAL_BRANCH=$(git rev-parse --abbrev-ref HEAD)
    if [ "$ORIGINAL_BRANCH" = "HEAD" ]; then
        # Detached HEAD state, use commit hash
        ORIGINAL_BRANCH=$(git rev-parse HEAD)
    fi
    log "Current branch: $ORIGINAL_BRANCH"
    echo ""

    # Check if base branch exists
    if ! git rev-parse --verify "$BASE_BRANCH" &>/dev/null; then
        error "Base branch '$BASE_BRANCH' does not exist"
    fi

    # Stash any uncommitted changes
    if ! git diff --quiet || ! git diff --cached --quiet; then
        log "Stashing uncommitted changes"
        git stash push -m "local-benchmark-stash-$(date +%s)"
        STASH_CREATED=true
        echo ""
    fi

    # Copy benchmark scripts to temp location (they may not exist on base branch)
    log "Copying benchmark scripts to temp location"
    TEMP_SCRIPTS_DIR=$(mktemp -d)
    cp "$SCRIPT_DIR/run_fx_benchmarks.sh" "$TEMP_SCRIPTS_DIR/"
    cp "$SCRIPT_DIR/run_snapshot_benchmark.sh" "$TEMP_SCRIPTS_DIR/"
    chmod +x "$TEMP_SCRIPTS_DIR"/*.sh
    echo "  Scripts copied to $TEMP_SCRIPTS_DIR"
    echo ""

    # === BUILD BASE BRANCH ===
    log "Building $BASE_BRANCH branch"
    git checkout "$BASE_BRANCH" --quiet

    echo "  Running: zig build release snapshot -Doptimize=ReleaseFast"
    if ! zig build release snapshot -Doptimize=ReleaseFast; then
        error "Failed to build $BASE_BRANCH branch"
    fi

    mkdir -p bench-main
    cp zig-out/bin/roc bench-main/roc
    cp zig-out/bin/snapshot bench-main/snapshot
    echo "  Built binaries copied to bench-main/"
    echo ""

    # === BUILD CURRENT BRANCH ===
    log "Building $ORIGINAL_BRANCH branch"
    git checkout "$ORIGINAL_BRANCH" --quiet

    echo "  Running: zig build release snapshot -Doptimize=ReleaseFast"
    if ! zig build release snapshot -Doptimize=ReleaseFast; then
        error "Failed to build $ORIGINAL_BRANCH branch"
    fi

    mkdir -p bench-local
    cp zig-out/bin/roc bench-local/roc
    cp zig-out/bin/snapshot bench-local/snapshot
    echo "  Built binaries copied to bench-local/"
    echo ""

    # === RUN BENCHMARKS ===
    if [ "$SKIP_FX" = "false" ]; then
        log "Running FX benchmarks"
        "$TEMP_SCRIPTS_DIR/run_fx_benchmarks.sh" bench-main/roc bench-local/roc "$BASE_BRANCH" "$ORIGINAL_BRANCH"
        echo ""
    fi

    if [ "$SKIP_SNAPSHOT" = "false" ]; then
        log "Running snapshot benchmarks"
        "$TEMP_SCRIPTS_DIR/run_snapshot_benchmark.sh" bench-main/snapshot bench-local/snapshot "$BASE_BRANCH" "$ORIGINAL_BRANCH"
        echo ""
    fi

    # === SUMMARY ===
    log "Benchmark Complete"
    echo "  Base branch: $BASE_BRANCH"
    echo "  Test branch: $ORIGINAL_BRANCH"
    echo "  No significant regressions detected"
}

main "$@"
