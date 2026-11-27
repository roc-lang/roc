#!/usr/bin/env bash
# Entry point for the range-prove model. See README.md.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(git -C "$HERE" rev-parse --show-toplevel)"
ROC="${ROC:-$REPO/zig-out/bin/roc}"
FX_PLATFORM="${FX_PLATFORM:-$REPO/test/fx/platform/main.roc}"
OUT="$HERE/out"
PY="${PYTHON:-python3}"

usage() {
    cat <<EOF
usage: ./run.sh <command>

  selftest                     validator fixtures, then one generated u8 program end to end
  check <file.roc>...          build each app at --opt=speed and re-prove every rewrite
  fuzz [N] [--seed S] [--domain general|u8|u16]
                               generate N programs (default 20), compare dev and speed
                               builds, and re-prove every rewrite
  prove <dump>... [rpv.py options]
                               re-prove the rewrites in existing ROC_RANGE_PROVE_DUMP output

ROC selects the compiler (now: $ROC); it must read ROC_RANGE_PROVE_DUMP.
FX_PLATFORM selects the platform generated programs use (now: $FX_PLATFORM).
Output goes to $OUT.
EOF
}

need() {
    if ! "$PY" -c 'import z3' 2>/dev/null; then
        echo "error: the z3 Python package is missing: pip install z3-solver" >&2
        exit 2
    fi
    if [[ "${1:-}" == roc && ! -x "$ROC" ]]; then
        echo "error: no compiler at $ROC (set ROC)" >&2
        exit 2
    fi
}

cmd="${1:-}"
[[ $# -gt 0 ]] && shift
case "$cmd" in
    selftest)
        need roc
        "$PY" "$HERE/selftest.py" "$OUT/selftest"
        echo "end to end: one generated program (u8 domain, seed 1)"
        "$PY" "$HERE/driver.py" fuzz "$ROC" "$OUT/selftest" "$FX_PLATFORM" 1 --seed 1 --domain u8
        echo "selftest passed"
        ;;
    check)
        [[ $# -gt 0 ]] || { usage; exit 2; }
        need roc
        "$PY" "$HERE/driver.py" check "$ROC" "$OUT/check" "$@"
        ;;
    fuzz)
        need roc
        n=20
        if [[ $# -gt 0 && "$1" != -* ]]; then n="$1"; shift; fi
        "$PY" "$HERE/driver.py" fuzz "$ROC" "$OUT/fuzz" "$FX_PLATFORM" "$n" "$@"
        ;;
    prove)
        [[ $# -gt 0 ]] || { usage; exit 2; }
        need
        "$PY" "$HERE/rpv.py" "$@"
        ;;
    *)
        usage
        [[ -z "$cmd" ]] && exit 0
        exit 2
        ;;
esac
