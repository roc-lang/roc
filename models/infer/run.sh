#!/usr/bin/env bash
# Entry point for the infer model. See README.md.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROC="${ROC:-$(git -C "$here" rev-parse --show-toplevel)/zig-out/bin/roc}"
OUT="$here/out"
export ROC OUT

usage() {
    cat <<'USAGE'
usage: ./run.sh <command>

  selftest                       parser round trip, then the fixtures in tests/ against the compiler
  check <file.roc>...            run the model and the compiler on fragment programs and compare
  fuzz [N] [--seed S] [-j J]     generate N modules (default 100) and their variants and compare

ROC selects the compiler (default: zig-out/bin/roc at the repository root). Output goes to out/.
USAGE
}

if [ $# -eq 0 ]; then
    usage
    exit 0
fi

case "$1" in
    selftest | check | fuzz) ;;
    -h | --help | help) usage; exit 0 ;;
    *) usage >&2; exit 2 ;;
esac

if [ ! -x "$ROC" ]; then
    echo "roc not found at $ROC; build it with 'zig build roc' or set ROC" >&2
    exit 2
fi

mkdir -p "$OUT"
exec python3 "$here/main.py" "$@"
