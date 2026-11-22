#!/usr/bin/env bash
# Entry point for the error-recovery model. See README.md.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROC="${ROC:-$(git -C "$here" rev-parse --show-toplevel)/zig-out/bin/roc}"
OUT="${OUT:-$here/out}"
export ROC OUT

usage() {
    cat <<'USAGE'
usage: ./run.sh <command>

  selftest                       render 300 generated programs and their mutants, then run the fixtures in tests/
  check <file.roc>...            run check, test and build in every mode on each file and judge the results
  fuzz [N] [--seed S] [-j J]     generate N programs (default 100), mutate each, and judge every mutant
       [--variants V] [--size F] [--no-reduce] [--no-run] [--commands C,...]

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
