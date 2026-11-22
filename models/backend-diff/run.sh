#!/usr/bin/env bash
# Entry point for the backend-diff model. See README.md.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROC="${ROC:-$(git -C "$here" rev-parse --show-toplevel)/zig-out/bin/roc}"
NODE="${NODE:-node}"
OUT="$here/out"
export ROC NODE OUT

usage() {
	cat <<USAGE
usage: ./run.sh <command>

  selftest                  reference-only generator batch, the fixtures in tests/, a small fuzz batch
  check <dir>...            run each directory's app.roc (and wasm/app.roc, if present) on every path;
                            compare with the directory's expected.txt, or with each other without one
  fuzz [N] [--seed S] [--configs a,b] [--no-reduce] [--keep]
                            generate N programs (default 20), compare every path with the reference,
                            reduce each new disagreement
  reduce <seed>             regenerate program <seed> and reduce its disagreement
  gen <seed> [--target native|wasm]
                            print program <seed>, its expected output and its cross-module tail-group calls

ROC selects the compiler (now: $ROC). NODE runs wasm32 output. Output goes to out/.
USAGE
}

need_roc() {
	if [[ ! -x "$ROC" ]]; then
		echo "roc not found at $ROC; build it with 'zig build roc' or set ROC" >&2
		exit 2
	fi
}

need_wasm_host() {
	if [[ ! -f "$here/platform/targets/wasm32/host.wasm" ]]; then
		echo "platform/targets/wasm32/host.wasm is missing; see README.md (Requirements)" >&2
		exit 2
	fi
	command -v "$NODE" >/dev/null || { echo "node not found; set NODE" >&2; exit 2; }
}

cmd="${1:-}"
[[ $# -gt 0 ]] && shift
case "$cmd" in
	selftest)
		need_roc
		need_wasm_host
		mkdir -p "$OUT"
		python3 -I "$here/driver.py" selftest "$here"/tests/*/
		echo "selftest passed"
		;;
	check)
		[[ $# -gt 0 ]] || { usage; exit 2; }
		need_roc
		need_wasm_host
		python3 -I "$here/driver.py" check "$@"
		;;
	fuzz)
		need_roc
		need_wasm_host
		python3 -I "$here/driver.py" fuzz "$@"
		;;
	reduce)
		need_roc
		need_wasm_host
		python3 -I "$here/driver.py" reduce "$@"
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
