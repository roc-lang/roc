#!/bin/bash
# Run multiple AFL++ fuzz-typecheck instances, leaving 2 CPUs free

set -e

CORPUS_DIR="/tmp/typecheck-corpus"
OUTPUT_DIR="/tmp/typecheck-out"
FUZZER="zig-out/bin/fuzz-typecheck"
RESERVED_CPUS=2

# Get number of CPUs to use
TOTAL_CPUS=$(nproc)
NUM_FUZZERS=$((TOTAL_CPUS - RESERVED_CPUS))

if [ "$NUM_FUZZERS" -lt 1 ]; then
    NUM_FUZZERS=1
fi

start_fuzzers() {
    echo "Starting $NUM_FUZZERS fuzzer(s) (reserving $RESERVED_CPUS CPUs)..."

    # Create corpus directory with seeds if it doesn't exist
    if [ ! -d "$CORPUS_DIR" ] || [ -z "$(ls -A $CORPUS_DIR 2>/dev/null)" ]; then
        echo "Creating seed corpus..."
        mkdir -p "$CORPUS_DIR"
        for i in $(seq 1 100); do
            head -c 8 /dev/urandom > "$CORPUS_DIR/seed$i"
        done
    fi

    # AFL++ environment settings
    export AFL_SKIP_CPUFREQ=1
    export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1

    # Start main fuzzer
    echo "Starting main fuzzer..."
    afl-fuzz -M main -i "$CORPUS_DIR" -o "$OUTPUT_DIR" "$FUZZER" >/dev/null 2>&1 &
    echo "  main (PID: $!)"

    # Start secondary fuzzers
    for i in $(seq 1 $((NUM_FUZZERS - 1))); do
        sleep 0.5  # Stagger startup
        afl-fuzz -S "secondary$i" -i "$CORPUS_DIR" -o "$OUTPUT_DIR" "$FUZZER" >/dev/null 2>&1 &
        echo "  secondary$i (PID: $!)"
    done

    echo ""
    echo "All fuzzers started. Monitor with:"
    echo "  watch -n 5 afl-whatsup $OUTPUT_DIR"
    echo ""
    echo "Stop with:"
    echo "  $0 stop"
}

stop_fuzzers() {
    echo "Stopping all fuzz-typecheck instances..."
    pkill -f "afl-fuzz.*fuzz-typecheck" 2>/dev/null || true
    pkill -f "zig-out/bin/fuzz-typecheck" 2>/dev/null || true
    sleep 1

    # Check if any remain
    if pgrep -f "fuzz-typecheck" >/dev/null 2>&1; then
        echo "Some processes still running, sending SIGKILL..."
        pkill -9 -f "afl-fuzz.*fuzz-typecheck" 2>/dev/null || true
        pkill -9 -f "zig-out/bin/fuzz-typecheck" 2>/dev/null || true
    fi

    echo "All fuzzers stopped."
}

status_fuzzers() {
    if command -v afl-whatsup >/dev/null 2>&1; then
        afl-whatsup "$OUTPUT_DIR" 2>/dev/null || echo "No fuzzers running or no output yet."
    else
        echo "Fuzzer processes:"
        pgrep -a -f "fuzz-typecheck" || echo "No fuzzers running."
    fi
}

case "${1:-start}" in
    start)
        start_fuzzers
        ;;
    stop)
        stop_fuzzers
        ;;
    status)
        status_fuzzers
        ;;
    restart)
        stop_fuzzers
        sleep 2
        start_fuzzers
        ;;
    *)
        echo "Usage: $0 {start|stop|status|restart}"
        exit 1
        ;;
esac
