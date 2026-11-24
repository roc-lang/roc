#!/usr/bin/env bash
# Entry point for the cache-incremental model. See README.md.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROC="${ROC:-$(git -C "$here" rev-parse --show-toplevel)/zig-out/bin/roc}"
OUT="$here/out"
export ROC OUT

usage() {
	cat <<USAGE
usage: ./run.sh <command>

  selftest                  generator and edit check, the fixtures in tests/, generated states check cleanly
  fuzz [N] [--seed S] [--steps K] [--configs a,b] [--jobs J] [--keep] [--no-verify] [--no-llvm]
                            N edit sequences (default 10) of K edits (default 12); after each edit every
                            config runs with the sequence's warm cache and with an empty one, and must agree
  replay <dir> [--configs a,b] [--states i,j]
                            rebuild a saved sequence (out/fuzz/<seed>, or a reduced min/) with a fresh warm cache
  reduce <dir>              shrink a failing sequence to the fewest states and configs; writes <dir>/min
  check <dir>...            replay fixture sequences; each must agree
  gen <seed> [--steps K] [--all]
                            print a sequence's edits and its first state's files

Configs: check, test, test-spec, run, run-spec, interp, dev, dev-spec, speed, size, size-boxy.
ROC selects the compiler (now: $ROC). Output goes to out/.
USAGE
}

need_roc() {
	if [[ ! -x "$ROC" ]]; then
		echo "roc not found at $ROC; build it with 'zig build roc' or set ROC" >&2
		exit 2
	fi
}

cmd="${1:-}"
[[ $# -gt 0 ]] && shift
case "$cmd" in
	selftest)
		need_roc
		mkdir -p "$OUT"
		python3 -I "$here/driver.py" selftest "$here"/tests/*/
		echo "selftest passed"
		;;
	fuzz | replay | reduce | check)
		need_roc
		python3 -I "$here/driver.py" "$cmd" "$@"
		;;
	gen)
		python3 -I "$here/driver.py" gen "$@"
		;;
	*)
		usage
		[[ -z "$cmd" ]] && exit 0
		exit 2
		;;
esac
