#!/usr/bin/env bash
# Entry point for the SpecConstr model. See README.md.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROC="${ROC:-$(git -C "$here" rev-parse --show-toplevel)/zig-out/bin/roc}"
MAUDE="${MAUDE:-maude}"
OUT="$here/out"
export ROC MAUDE OUT

usage() {
	cat <<USAGE
usage: ./run.sh <command>

  selftest              Maude suites, a model-only fuzz batch, check on tests/*.roc, a small fuzz
  check <file.roc>...   run each file's expects with roc test at --opt=dev and --opt=speed
  fuzz [N] [--seed S] [--family model|wide|both] [--no-roc]
                        generate N programs (default 40) and compare against roc test

ROC selects the compiler (now: $ROC). MAUDE selects maude. Output goes to out/.
USAGE
}

need_roc() {
	if [[ ! -x "$ROC" ]]; then
		echo "roc not found at $ROC; build it with 'zig build roc' or set ROC" >&2
		exit 2
	fi
}

need_maude() {
	if ! command -v "$MAUDE" >/dev/null; then
		echo "maude not found; see README.md for install steps, or set MAUDE" >&2
		exit 2
	fi
}

cmd="${1:-}"
[[ $# -gt 0 ]] && shift
case "$cmd" in
	selftest)
		need_maude
		need_roc
		mkdir -p "$OUT"
		echo "== Maude suites"
		python3 -I "$here/driver.py" maude-tests "$here"/tests/*.maude
		echo "== model check on generated programs"
		python3 -I "$here/driver.py" fuzz 200 --seed 1 --family model --no-roc
		echo "== check"
		python3 -I "$here/driver.py" check "$here"/tests/*.roc
		echo "== fuzz against roc"
		python3 -I "$here/driver.py" fuzz 20 --seed 1
		echo "selftest passed"
		;;
	check)
		[[ $# -gt 0 ]] || { usage; exit 2; }
		need_roc
		python3 -I "$here/driver.py" check "$@"
		;;
	fuzz)
		need_maude
		if [[ " $* " != *" --no-roc "* ]]; then need_roc; fi
		python3 -I "$here/driver.py" fuzz "$@"
		;;
	*)
		usage
		[[ -z "$cmd" ]] && exit 0
		exit 2
		;;
esac
