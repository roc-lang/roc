#!/usr/bin/env bash
# Entry point for the codec-roundtrip model. See README.md.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROC="${ROC:-$(git -C "$here" rev-parse --show-toplevel)/zig-out/bin/roc}"
NODE="${NODE:-node}"
OUT="$here/out"
export ROC NODE OUT

usage() {
	cat <<USAGE
usage: ./run.sh <command>

  selftest [--configs a,b]  reference-encoder unit checks, the fixtures in tests/, a 2-program fuzz batch
  check <dir>... [--configs a,b]
                            run each fixture directory's case.py on every config; a config named in its
                            known.txt must disagree, every other must agree
  fuzz [N] [--seed S] [--cases K] [--configs a,b] [--keep] [--no-reduce]
                            generate N programs of K cases (default 10 and 5), check every config against
                            the model, reduce each disagreement to one case
  reduce <seed> [--cases K] regenerate program <seed> and reduce its disagreement
  gen <seed> [--cases K]    print program <seed> as a test module, and the app output the model expects

Configs: test-dev test-dev-spec test-speed test-speed-boxy run-interp dev dev-spec speed speed-boxy
wasm-dev wasm-dev-spec wasm-speed (default: all). ROC selects the compiler (now: $ROC). Output goes to out/.
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
		python3 -I "$here/driver.py" selftest "$here"/tests/*/ "$@"
		echo "selftest passed"
		;;
	check | fuzz | reduce)
		need_roc
		need_wasm_host
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
