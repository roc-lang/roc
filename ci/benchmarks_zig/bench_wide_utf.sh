#!/usr/bin/env bash
set -euo pipefail

# Compare the working-tree wide-UTF decoders against a revision that provides
# fromUtf16/fromUtf32 (for example a93a0863be, the initial implementation).
# No checkout or tracked file is changed. CSV is written to stderr.
BASE_REF="${1:?Usage: bench_wide_utf.sh BASE_REF}"
case "$(uname -s)" in
    Darwin|Linux) ;;
    *) echo "This benchmark requires a POSIX libc host." >&2; exit 1 ;;
esac
REPO_ROOT="$(git rev-parse --show-toplevel)"
PERF_DIR="$(mktemp -d "${TMPDIR:-/tmp}/roc-wide-utf.XXXXXX")"
trap 'rm -rf "$PERF_DIR"' EXIT
cd "$REPO_ROOT"
git archive "$BASE_REF" src/builtins | tar -x -C "$PERF_DIR"
printf 'pub const enable_tracy: bool = false;\n' > "$PERF_DIR/options.zig"
zig build-exe -O ReleaseFast -lc --dep current --dep baseline \
    -Mroot=ci/benchmarks_zig/bench_wide_utf.zig \
    --dep tracy --dep vendor_parse_float --dep vendor_ryu --dep roc_str_view \
    -Mcurrent=src/builtins/mod.zig \
    --dep tracy --dep vendor_parse_float --dep vendor_ryu --dep roc_str_view \
    -Mbaseline="$PERF_DIR/src/builtins/mod.zig" \
    --dep build_options -Mtracy=src/build/tracy.zig \
    -Mvendor_parse_float=vendor/parse_float/parse_float.zig \
    -Mvendor_ryu=vendor/ryu.zig \
    -Mroc_str_view=src/default_platform/roc_str_view.zig \
    -Mbuild_options="$PERF_DIR/options.zig" \
    -femit-bin="$PERF_DIR/bench"
"$PERF_DIR/bench"
