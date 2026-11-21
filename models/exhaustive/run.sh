#!/usr/bin/env bash
# Entry point for the exhaustiveness model. See README.md.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROC="${ROC:-$(git -C "$here" rev-parse --show-toplevel)/zig-out/bin/roc}"
export ROC

usage() {
    cat <<USAGE
usage: ./run.sh <command> [args]

  selftest                          model unit tests, generator checks, and the tests/ fixtures (needs roc)
  check <case.roc>...               re-run the comparison on case files written by fuzz or reduce
  fuzz [N] [--seed S] [--ctx C] [--jobs J]
                                    generate N programs (default 200) and compare roc check with the model
  reduce <case.roc>                 shrink a non-ok case file; writes out/reduced.roc

ROC selects the compiler (now: $ROC). Output goes to out/.
USAGE
}

if [ $# -eq 0 ]; then
    usage
    exit 0
fi

case "$1" in
    selftest | check | fuzz | reduce) exec python3 "$here/cli.py" "$@" ;;
    -h | --help | help) usage ;;
    *) usage >&2; exit 2 ;;
esac
