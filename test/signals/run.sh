#!/usr/bin/env bash
set -euo pipefail

# Build and run the signals test app.
#
# Usage:
#   ./run.sh                        # build + run with test_counter.txt
#   ./run.sh test_hello.txt          # build + run with specific test spec
#   ./run.sh -v                      # build + run verbose
#   ./run.sh --debug                 # build + run under lldb
#   ./run.sh --host-only             # only rebuild platform host
#   ./run.sh --app-only              # only rebuild app (skip host)
#   ./run.sh --no-build              # skip all builds, just run
#   ./run.sh --disassemble           # build + disassemble roc__main

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ROC="$REPO_ROOT/zig-out/bin/roc"
APP="$SCRIPT_DIR/app"
APP_ROC="$SCRIPT_DIR/app.roc"
ROC_CACHE="$HOME/Library/Caches/roc"
DEFAULT_TEST_SPEC="$SCRIPT_DIR/test_counter.txt"

# Parse args
BUILD_HOST=true
BUILD_APP=true
RUN_APP=true
VERBOSE=""
DEBUG=false
DISASSEMBLE=false
TEST_SPEC=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -v|--verbose)
            VERBOSE="--verbose"
            shift
            ;;
        --debug)
            DEBUG=true
            shift
            ;;
        --host-only)
            BUILD_APP=false
            RUN_APP=false
            shift
            ;;
        --app-only)
            BUILD_HOST=false
            shift
            ;;
        --no-build)
            BUILD_HOST=false
            BUILD_APP=false
            shift
            ;;
        --disassemble)
            DISASSEMBLE=true
            RUN_APP=false
            shift
            ;;
        -h|--help)
            echo "Usage: ./run.sh [OPTIONS] [test_spec.txt]"
            echo ""
            echo "Options:"
            echo "  -v, --verbose      Run app with verbose output"
            echo "  --debug            Run under lldb"
            echo "  --host-only        Only rebuild platform host"
            echo "  --app-only         Only rebuild app (skip host)"
            echo "  --no-build         Skip all builds, just run"
            echo "  --disassemble      Build and disassemble roc__main"
            echo "  -h, --help         Show this help"
            echo ""
            echo "If no test spec is given, defaults to test_counter.txt"
            exit 0
            ;;
        *)
            TEST_SPEC="$1"
            shift
            ;;
    esac
done

# Resolve test spec path
if [[ -z "$TEST_SPEC" ]]; then
    TEST_SPEC="$DEFAULT_TEST_SPEC"
elif [[ ! "$TEST_SPEC" = /* ]]; then
    # Relative path - resolve from script dir
    TEST_SPEC="$SCRIPT_DIR/$TEST_SPEC"
fi

# --- Step 1: Build compiler (check if roc binary exists) ---
if [[ ! -f "$ROC" ]]; then
    echo "==> Roc compiler not found at $ROC"
    echo "    Run 'zig build' from $REPO_ROOT first"
    exit 1
fi

# --- Step 2: Build platform host ---
if $BUILD_HOST; then
    echo "==> Building platform host..."
    cd "$SCRIPT_DIR"
    zig build 2>&1
    echo "    Done."
    cd "$REPO_ROOT"
fi

# --- Step 3: Build app ---
if $BUILD_APP; then
    echo "==> Clearing roc cache..."
    rm -rf "$ROC_CACHE"
    rm -f "$APP"

    echo "==> Building app (dev backend)..."
    "$ROC" build --backend=dev "$APP_ROC" 2>&1
fi

# --- Step 4: Check binary exists ---
if [[ ! -f "$APP" ]]; then
    echo "ERROR: App binary not found at $APP"
    echo "       (roc build may have placed it elsewhere)"
    # Check common alternative locations
    if [[ -f "$REPO_ROOT/app" ]]; then
        echo "       Found at $REPO_ROOT/app — moving it"
        mv "$REPO_ROOT/app" "$APP"
    else
        exit 1
    fi
fi

# --- Step 5: Disassemble ---
if $DISASSEMBLE; then
    echo "==> Disassembling roc__main..."
    lldb "$APP" -o "disassemble -n roc__main" -o "quit" 2>&1
    exit 0
fi

# --- Step 6: Run ---
TIMEOUT_SECS=5

if $RUN_APP; then
    if $DEBUG; then
        echo "==> Running under lldb..."
        echo "    (use 'run' to start, Ctrl+C to interrupt, 'bt' for backtrace)"
        lldb "$APP" -- $VERBOSE "$TEST_SPEC"
    else
        echo "==> Running: $APP $VERBOSE $TEST_SPEC"
        # Run with a timeout — the test runner is pure simulation, should finish in <1s.
        # If it hangs, we kill it rather than blocking forever.
        "$APP" $VERBOSE "$TEST_SPEC" &
        APP_PID=$!
        ( sleep $TIMEOUT_SECS && kill -9 $APP_PID 2>/dev/null && echo "" && echo "==> TIMEOUT: app hung for ${TIMEOUT_SECS}s — killed (pid $APP_PID)" >&2 ) &
        WATCHDOG_PID=$!
        wait $APP_PID 2>/dev/null
        EXIT_CODE=$?
        # Kill the watchdog if the app finished in time
        kill $WATCHDOG_PID 2>/dev/null
        wait $WATCHDOG_PID 2>/dev/null || true

        if [[ $EXIT_CODE -eq 137 ]]; then
            echo "==> HANG detected (killed by timeout after ${TIMEOUT_SECS}s)"
            exit 1
        elif [[ $EXIT_CODE -eq 0 ]]; then
            echo "==> PASS (exit 0)"
        else
            echo "==> FAIL (exit $EXIT_CODE)"
        fi
        exit $EXIT_CODE
    fi
fi
