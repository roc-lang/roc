#!/usr/bin/env bash
# Entry point for the boxy-ctfe model. See README.md.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROC="${ROC:-$(git -C "$here" rev-parse --show-toplevel)/zig-out/bin/roc}"
OUT="$here/out"
export ROC OUT

usage() {
	cat <<USAGE
usage: ./run.sh <command>

  selftest                  generator determinism, the fixtures in tests/, a 3-program fuzz batch
  check <dir>...            run each directory's app.roc on every configuration; every probe's sides must
                            agree, every configuration must print the same, and expected.txt (if present) too
  fuzz [N] [--seed S] [--configs a,b] [--no-reduce] [--keep]
                            generate N programs (default 20) and judge each; reduce each failure
  reduce <seed|prog.pickle> [--configs a,b]
                            regenerate a program (or load a saved one) and reduce its failure
  gen <seed>                print program <seed>
  reach <seed>              list the compile-time roots \`roc check\` evaluates for program <seed>

ROC selects the compiler (now: $ROC). JOBS (default 4) is how many configurations run at once.
Output goes to out/.
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
		python3 -I "$here/driver.py" selftest "$here"/tests/*/ "$@"
		echo "selftest passed"
		;;
	check | fuzz | reduce | reach)
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
