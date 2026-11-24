#!/usr/bin/env bash
# Entry point for the match-compile model. See README.md.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROC="${ROC:-$(git -C "$here" rev-parse --show-toplevel)/zig-out/bin/roc}"
NODE="${NODE:-node}"
OUT="${OUT:-$here/out}"
export ROC NODE OUT

usage() {
	cat <<USAGE
usage: ./run.sh <command> [args]

  selftest [--configs a,b]          model unit tests, generator checks, the fixtures in tests/
  fuzz [N] [--seed S] [--configs a,b] [--jobs J] [--no-reduce]
                                    generate N programs (default 20), run each on every config,
                                    compare with the model, reduce each disagreement
  check <case.txt>... [--configs a,b]
                                    rerun saved programs (from out/fuzz/<seed>/ or tests/<name>/)
  reduce <case.txt> [--configs a,b]
                                    shrink a saved program while the same configs disagree
  gen <seed> [--target native|wasm|test]
                                    print the program for a seed

Configs: interp test dev dev-spec speed speed-spec wasm-dev wasm-dev-spec wasm-speed wasm-speed-spec.
ROC selects the compiler (now: $ROC). NODE runs wasm32 output. JOBS (default 3) is how many configs
of one program build at once. Output goes to out/.
USAGE
}

need_tools() {
	[[ -x "$ROC" ]] || { echo "roc not found at $ROC; build it with 'zig build roc' or set ROC" >&2; exit 2; }
	[[ -f "$here/platform/targets/wasm32/host.wasm" ]] || {
		echo "platform/targets/wasm32/host.wasm is missing; see README.md (Requirements)" >&2
		exit 2
	}
	command -v "$NODE" >/dev/null || { echo "node not found; set NODE" >&2; exit 2; }
}

cmd="${1:-}"
[[ $# -gt 0 ]] && shift
case "$cmd" in
	selftest | fuzz | check | reduce)
		need_tools
		mkdir -p "$OUT"
		exec python3 -I -u "$here/driver.py" "$cmd" "$@"
		;;
	gen) exec python3 -I "$here/driver.py" gen "$@" ;;
	-h | --help | help | "") usage ;;
	*) usage >&2; exit 2 ;;
esac
