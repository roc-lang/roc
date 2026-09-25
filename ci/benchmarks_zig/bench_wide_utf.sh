#!/usr/bin/env bash
set -euo pipefail

# End-to-end public API benchmark. Pass two built Roc compilers to compare
# revisions, or one to measure it. Includes startup/input construction; use
# enough iterations to amortize those costs. Does not change any checkout.
# Example: bench_wide_utf.sh /tmp/roc-before zig-out/bin/roc
if [[ $# -lt 1 || $# -gt 2 ]]; then
    echo "Usage: $0 ROC_BINARY [OTHER_ROC_BINARY]" >&2
    exit 1
fi
REPO_ROOT="$(git rev-parse --show-toplevel)"
PERF_DIR="$(mktemp -d "${TMPDIR:-/tmp}/roc-wide-utf.XXXXXX")"
trap 'rm -rf "$PERF_DIR"' EXIT
for compiler in "$@"; do
    binary="$PERF_DIR/bench"
    "$compiler" build --opt=speed --no-cache --output="$binary" "$REPO_ROOT/ci/benchmarks_zig/bench_wide_utf.roc"
    python3 - "$binary" "$compiler" <<'PY'
import statistics
import subprocess
import sys
import time
binary, compiler = sys.argv[1:]
print('compiler,width,mode,case,units,iterations,median_ms')
for width in ('16', '32'):
    for mode in ('strict', 'lossy'):
        for case in ('ascii', 'mostly_ascii', 'bmp', 'supplementary', 'invalid_tail', 'invalid_head'):
            samples = []
            outputs = []
            for _ in range(5):
                start = time.perf_counter_ns()
                outputs.append(subprocess.check_output([binary, width, mode, case, '1048576', '64']))
                samples.append((time.perf_counter_ns() - start) / 1e6)
            assert len(set(outputs)) == 1, 'non-deterministic output'
            print(f'{compiler},{width},{mode},{case},1048576,64,{statistics.median(samples):.3f}', flush=True)
PY
done
