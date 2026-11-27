#!/usr/bin/env bash
# Entry point for the ARC model. Run `./run.sh` for usage.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(git -C "$HERE" rev-parse --show-toplevel)"
ROC="${ROC:-$REPO/zig-out/bin/roc}"
case "$ROC" in /*) ;; *) ROC="$PWD/$ROC" ;; esac
OUT="$HERE/out"
BIN="$HERE/lean/.lake/build/bin/rocarc"

# Exploration limits for the heap machine, per root. R-Equiv runs every root
# twice, so it forks less.
LIMITS=(--steps 40000 --paths 200 --forks 4)
EQUIV_LIMITS=(--steps 40000 --paths 200 --forks 3)

usage() {
	cat <<'EOF'
usage: ./run.sh <command> [args]

  selftest                  build the Lean model, check the fixtures in tests/, and run
                            one small program through the compiler (needs $ROC)
  check FILE.roc...         compile each file with the LIR dump on (--specialize=yes and
                            --opt=speed) and run the model on every dump
  corpus [DIR...]           `check` every .roc file under DIR (default: test/cli)
  fuzz [N] [--seed S] [--size K] [--focus]
                            generate N programs (default 20) with gen.py, run them at
                            --opt=interpreter, dev, --specialize=yes and --opt=speed,
                            and run the model on the two dumps
  optable                   regenerate lean/RocArc/OpTable.lean from src/base/LowLevel.zig
                            (needs zig)

ROC selects the compiler (default: zig-out/bin/roc at the repository root).
Output goes to out/.
EOF
}

build() {
	(cd "$HERE/lean" && lake build -q) >&2
}

need_roc() {
	if [ ! -x "$ROC" ]; then
		echo "roc not found at $ROC (set ROC, or run \`zig build roc\`)" >&2
		exit 2
	fi
}

# The first line of `roc test` output that says how the run ended.
test_result() {
	grep -E "^All \(|tests? (passed|failed)|panic|Segmentation fault|invariant" "$1" \
		| grep -v "^ " | head -1 | cut -c1-80 || true
}

# Runs the model on one dump. Prints one finding per line; prints nothing when clean.
# Lines from the heap machine (oracle, equiv) are bugs in the compiler or the model.
# `certify:` lines are disagreements with the compiler's own certifier, which
# accepted the program (a Debug compiler always runs it); see README "Limits".
model_findings() {
	local dump="$1"
	{
		"$BIN" oracle "$dump" "${LIMITS[@]}" 2>&1 | grep -E "^  (STALE|USE-AFTER|DOUBLE|LEAK|SHARED)|parse error" | sed 's/^ */oracle: /' || true
		"$BIN" oracle "$dump" --checked "${LIMITS[@]}" 2>&1 | grep -E "^  (STALE|USE-AFTER|DOUBLE|LEAK|SHARED)|parse error" | sed 's/^ */oracle --checked: /' || true
		"$BIN" equiv "$dump" "${EQUIV_LIMITS[@]}" 2>&1 | grep -E "MISMATCH|parse error" | sed 's/^/equiv: /' || true
		"$BIN" certify "$dump" 2>&1 | grep -E "REJECTED|parse error" | cut -c1-240 | sed 's/^/certify: /' || true
	} | sort -u
}

# Compiles FILE in one mode with the LIR dump on and checks the dump.
# Writes DIR/MODE.log; prints a summary line and any findings; returns 1 on a finding.
check_mode() {
	local file="$1" dir="$2" mode="$3" flags
	case "$mode" in
		spec) flags="--specialize=yes" ;;
		speed) flags="--opt=speed" ;;
	esac
	local log="$dir/$mode.log" rc=0
	(cd "$(dirname "$file")" && ROC_VERIFY_ARC=1 ROC_LIR_DUMP= "$ROC" test --no-cache $flags "$(basename "$file")") >"$log" 2>&1 || rc=$?
	local result found=0
	result="$(test_result "$log")"
	echo "  $mode ($flags): exit=$rc ${result:-no test result}"
	if grep -qE "panic|Segmentation fault|invariant" "$log"; then
		echo "    compiler: $(grep -m1 -E "panic|Segmentation fault|invariant" "$log" | cut -c1-200)"
		found=1
	fi
	local f
	f="$(model_findings "$log")"
	if [ -n "$f" ]; then
		echo "$f" | sed 's/^/    /'
		if echo "$f" | grep -qv "^certify:"; then found=1; fi
	fi
	return $found
}

cmd_check() {
	[ $# -gt 0 ] || { usage; exit 2; }
	need_roc
	build
	local bad=0 file dir
	for file in "$@"; do
		file="$(cd "$(dirname "$file")" && pwd)/$(basename "$file")"
		dir="$OUT/check/$(echo "${file#"$REPO"/}" | tr '/' '_' | sed 's/\.roc$//')"
		mkdir -p "$dir"
		echo "$file"
		check_mode "$file" "$dir" spec || bad=$((bad + 1))
		check_mode "$file" "$dir" speed || bad=$((bad + 1))
	done
	echo "dumps in $OUT/check"
	[ $bad -eq 0 ]
}

cmd_corpus() {
	local dirs=("$@") files=()
	[ ${#dirs[@]} -gt 0 ] || dirs=("$REPO/test/cli")
	while IFS= read -r f; do files+=("$f"); done < <(find "${dirs[@]}" -name '*.roc' | sort)
	echo "${#files[@]} files"
	cmd_check "${files[@]}"
}

cmd_fuzz() {
	local n=20 seed=1 size=8 focus=""
	while [ $# -gt 0 ]; do
		case "$1" in
			--seed) seed="$2"; shift 2 ;;
			--size) size="$2"; shift 2 ;;
			--focus) focus="focus"; shift ;;
			*) n="$1"; shift ;;
		esac
	done
	need_roc
	build
	local bad=0 i s dir mode flags rc result line
	for ((i = 0; i < n; i++)); do
		s=$((seed + i))
		dir="$OUT/fuzz/s$s"
		mkdir -p "$dir"
		python3 -I "$HERE/gen.py" "$s" "$size" $focus >"$dir/prog.roc"
		line="seed=$s"
		local interesting=0
		for mode in interpreter dev spec speed; do
			case "$mode" in
				interpreter) flags="--opt=interpreter" ;;
				dev) flags="" ;;
				spec) flags="--specialize=yes" ;;
				speed) flags="--opt=speed" ;;
			esac
			rc=0
			if [ "$mode" = spec ] || [ "$mode" = speed ]; then
				ROC_VERIFY_ARC=1 ROC_LIR_DUMP= "$ROC" test --no-cache $flags "$dir/prog.roc" >"$dir/$mode.log" 2>&1 || rc=$?
			else
				"$ROC" test --no-cache $flags "$dir/prog.roc" >"$dir/$mode.log" 2>&1 || rc=$?
			fi
			result="$(test_result "$dir/$mode.log")"
			# Every check compares with gen.py's reference value, so all must pass.
			grep -q "^All (" "$dir/$mode.log" || interesting=1
			line="$line | $mode: ${result:-exit=$rc}"
			if [ "$mode" = spec ] || [ "$mode" = speed ]; then
				local f
				f="$(model_findings "$dir/$mode.log")"
				if [ -n "$f" ]; then
					if echo "$f" | grep -qv "^certify:"; then interesting=1; fi
					line="$line [$(echo "$f" | head -3 | tr '\n' ';')]"
				fi
			fi
		done
		echo "$line"
		if [ $interesting -eq 1 ]; then
			bad=$((bad + 1))
		else
			rm -rf "$dir"
		fi
	done
	echo "$n programs, $bad interesting (kept in $OUT/fuzz)"
	[ $bad -eq 0 ]
}

# expect DUMP COMMAND PATTERN: the model's COMMAND output on DUMP matches PATTERN
# (or matches nothing, if PATTERN is empty).
expect_out() {
	local dump="$1" cmd="$2" pattern="$3" out
	# shellcheck disable=SC2086
	out="$("$BIN" $cmd "$HERE/tests/$dump" 2>&1 || true)"
	local findings
	findings="$(echo "$out" | grep -E "STALE|USE-AFTER|DOUBLE|LEAK|SHARED|MISMATCH|REJECTED|parse error" || true)"
	if [ -z "$pattern" ]; then
		if [ -n "$findings" ]; then
			echo "FAIL $dump $cmd: expected no findings, got: $findings"
			return 1
		fi
	elif ! echo "$findings" | grep -q -- "$pattern"; then
		echo "FAIL $dump $cmd: expected $pattern, got: ${findings:-nothing}"
		return 1
	fi
	echo "ok   $dump $cmd ${pattern:-(clean)}"
}

cmd_selftest() {
	build
	local fail=0
	expect_out ok.dump oracle "" || fail=1
	expect_out ok.dump "oracle --checked" "" || fail=1
	expect_out ok.dump equiv "" || fail=1
	expect_out ok.dump certify "" || fail=1
	expect_out stale.dump oracle "STALE-REUSE" || fail=1
	expect_out hidden.dump oracle "" || fail=1
	expect_out hidden.dump equiv "VALUE-MISMATCH" || fail=1
	expect_out shared.dump oracle "SHARED-MUTATION" || fail=1
	expect_out shared.dump "oracle --checked" "" || fail=1
	expect_out shared.dump certify "C-UniqueStamp" || fail=1
	expect_out leak.dump oracle "LEAK" || fail=1
	expect_out leak.dump certify "C-Balance" || fail=1
	# the generator runs and is deterministic
	local s
	for s in 1 2 3 4 5 6 7 8 9 10; do
		python3 -I "$HERE/gen.py" "$s" 12 >/dev/null || { echo "FAIL gen.py seed $s"; fail=1; }
	done
	if [ "$(python3 -I "$HERE/gen.py" 3 12 | cksum)" = "$(python3 -I "$HERE/gen.py" 3 12 | cksum)" ]; then
		echo "ok   gen.py"
	else
		echo "FAIL gen.py is not deterministic"; fail=1
	fi
	# one real compilation, end to end
	need_roc
	local dir="$OUT/selftest"
	mkdir -p "$dir"
	if check_mode "$HERE/tests/smoke.roc" "$dir" spec && check_mode "$HERE/tests/smoke.roc" "$dir" speed \
		&& grep -q "^All (" "$dir/spec.log" && grep -q "^All (" "$dir/speed.log"; then
		echo "ok   tests/smoke.roc"
	else
		echo "FAIL tests/smoke.roc (logs in $dir)"; fail=1
	fi
	if [ $fail -eq 0 ]; then echo "selftest passed"; else echo "selftest FAILED"; fi
	return $fail
}

cmd_optable() {
	local cache="$OUT/zig-cache"
	mkdir -p "$cache"
	(cd "$REPO" && zig run --cache-dir "$cache" --dep lowlevel -Mroot="$HERE/tools/optable.zig" \
		-Mlowlevel=src/base/LowLevel.zig) 2>"$OUT/OpTable.lean"
	mv "$OUT/OpTable.lean" "$HERE/lean/RocArc/OpTable.lean"
	echo "wrote lean/RocArc/OpTable.lean"
}

main() {
	local cmd="${1:-}"
	[ $# -gt 0 ] && shift
	case "$cmd" in
		selftest) cmd_selftest "$@" ;;
		check) cmd_check "$@" ;;
		corpus) cmd_corpus "$@" ;;
		fuzz) cmd_fuzz "$@" ;;
		optable) cmd_optable "$@" ;;
		*) usage; [ -z "$cmd" ] && exit 0 || exit 2 ;;
	esac
}

main "$@"
