#!/usr/bin/env bash
# Entry point of the lifted-semantics model. See README.md.
#
#   ./run.sh selftest              interpreter unit checks, then the fixtures in tests/
#   ./run.sh check FILE.roc...     compile each file with the exports on and check it
#   ./run.sh fuzz N [--seed S]     generate N programs and check them
#   ./run.sh repo [GLOB]           check the repository's test programs (default test/**/*.roc)
#   ./run.sh dir DIR               check exports already written to DIR
set -euo pipefail
cd "$(dirname "$0")"
PY=${PYTHON:-python3}

case "${1:-}" in
selftest)
	$PY selftest.py
	$PY drive.py check tests/*.roc
	;;
check | fuzz | repo)
	$PY drive.py "$@"
	;;
dir)
	status=0
	$PY check.py "$2" || status=1
	$PY lsets.py "$2" || status=1
	exit $status
	;;
*)
	sed -n '2,9p' "$0"
	exit 2
	;;
esac
