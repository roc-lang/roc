#!/usr/bin/env bash
# Entry point for the LIR pass model. Run `./run.sh` for usage.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
ROC=${ROC:-$(git -C "$HERE" rev-parse --show-toplevel)/zig-out/bin/roc}
OUT=$HERE/out
PY=(python3 -I)

usage() {
  cat <<'EOF'
usage: ./run.sh <command>

  selftest                                 model self-test, then one compile of a fixture and of a generated program
  check [--rand N] [--all-stages] FILE...  compile each program with ROC_LIR_PASS_DUMP and check every pass
  fuzz [N] [--seed S]                      generate N programs (default 20, seeds S.., default 1), check every
                                           pass, and compare --opt=dev, --opt=speed and --specialize=yes output

ROC selects the compiler (default: zig-out/bin/roc at the repository root). It must read
ROC_LIR_PASS_DUMP (a Debug or -Ddev-diagnostics build). Output goes to out/.
EOF
}

need_roc() {
  if [ ! -x "$ROC" ]; then
    echo "error: no roc at $ROC (build one with 'zig build roc' or set ROC)" >&2
    exit 2
  fi
}

# compile FILE DUMP LOG: `roc test --opt=speed` with the dump on, from the program's directory so relative
# imports resolve. Sets STATUS to roc's exit status: 0 all tests passed, 2 passed with warnings, 1 a test
# failed or the program has errors, 128 and up a crash.
STATUS=0
compile() {
  local file=$1 dump=$2 log=$3
  rm -f "$dump"
  STATUS=0
  (cd "$(dirname "$file")" && ROC_LIR_PASS_DUMP="$dump" "$ROC" test --no-cache --opt=speed "$(basename "$file")") \
    >"$log" 2>&1 || STATUS=$?
  if passed && [ ! -s "$dump" ]; then
    echo "error: $ROC wrote no ROC_LIR_PASS_DUMP; it needs the lir_pass_dump developer diagnostic" >&2
    exit 2
  fi
}

passed() {
  [ "$STATUS" = 0 ] || [ "$STATUS" = 2 ]
}

cmd_selftest() {
  "${PY[@]}" "$HERE/tests/selftest.py"
  need_roc
  local dir=$OUT/selftest
  mkdir -p "$dir"

  compile "$HERE/tests/tce.roc" "$dir/tce.dump" "$dir/tce.log"
  if ! passed; then
    echo "FAIL roc test tests/tce.roc exited $STATUS (see $dir/tce.log)"
    exit 1
  fi
  "${PY[@]}" "$HERE/check.py" "$dir/tce.dump" >"$dir/tce.check" || { cat "$dir/tce.check"; echo "FAIL check tests/tce.roc"; exit 1; }
  echo "ok   tests/tce.roc compiles and checks clean: $(tail -1 "$dir/tce.check")"

  "${PY[@]}" "$HERE/gen.py" --seed 1 --groups 1 "$dir/gen1.roc"
  compile "$dir/gen1.roc" "$dir/gen1.dump" "$dir/gen1.log"
  if ! passed; then
    echo "FAIL roc test on a generated program exited $STATUS (see $dir/gen1.log)"
    exit 1
  fi
  "${PY[@]}" "$HERE/check.py" "$dir/gen1.dump" --all-stages >"$dir/gen1.check" || { cat "$dir/gen1.check"; echo "FAIL check gen1.roc"; exit 1; }
  echo "ok   generated program (seed 1) compiles and checks clean"
  echo "selftest: passed"
}

cmd_check() {
  local args=() files=() f key dir rc=0
  while [ $# -gt 0 ]; do
    case $1 in
      --rand) args+=(--rand "$2"); shift 2 ;;
      --all-stages) args+=(--all-stages); shift ;;
      -*) echo "unknown option $1" >&2; exit 2 ;;
      *) files+=("$1"); shift ;;
    esac
  done
  [ ${#files[@]} -gt 0 ] || { usage; exit 2; }
  need_roc
  dir=$OUT/check
  mkdir -p "$dir"
  for f in "${files[@]}"; do
    f=$(cd "$(dirname "$f")" && pwd)/$(basename "$f")
    key=$(echo "${f%.roc}" | sed 's#^/##; s#/#_#g')
    compile "$f" "$dir/$key.dump" "$dir/$key.log"
    if [ "$STATUS" -ge 128 ]; then
      echo "CRASH $f: roc exited $STATUS (see $dir/$key.log)"
      rc=1
      continue
    fi
    if [ ! -s "$dir/$key.dump" ]; then
      echo "skip  $f: roc exited $STATUS before lowering (see $dir/$key.log)"
      continue
    fi
    if "${PY[@]}" "$HERE/check.py" "$dir/$key.dump" ${args[@]+"${args[@]}"} >"$dir/$key.check"; then
      echo "ok    $f (roc exit $STATUS) $(tail -1 "$dir/$key.check")"
    else
      echo "FAIL  $f (roc exit $STATUS), findings in $dir/$key.check"
      grep '^FINDING' "$dir/$key.check" | head -5
      rc=1
    fi
  done
  return $rc
}

cmd_fuzz() {
  local n=20 seed=1 s last dir f bad=0
  while [ $# -gt 0 ]; do
    case $1 in
      --seed) seed=$2; shift 2 ;;
      -*) echo "unknown option $1" >&2; exit 2 ;;
      *) n=$1; shift ;;
    esac
  done
  need_roc
  dir=$OUT/fuzz
  mkdir -p "$dir"
  last=$((seed + n - 1))
  for s in $(seq "$seed" "$last"); do
    f=$dir/g$s
    "${PY[@]}" "$HERE/gen.py" --seed "$s" "$f.roc"
    compile "$f.roc" "$f.dump" "$f.log"
    if ! passed; then
      echo "seed $s: roc exited $STATUS (see $f.log)"
      bad=$((bad + 1))
      continue
    fi
    if ! "${PY[@]}" "$HERE/check.py" "$f.dump" --all-stages --rand 10 --seed "$s" >"$f.check"; then
      echo "seed $s: pass finding (see $f.check)"
      grep '^FINDING' "$f.check" | head -3
      bad=$((bad + 1))
      continue
    fi
    if ! "${PY[@]}" "$HERE/opt_diff.py" "$ROC" "$f.roc" >"$f.diff"; then
      echo "seed $s: output differs between opt levels (see $f.diff)"
      bad=$((bad + 1))
      continue
    fi
    rm -f "$f.roc" "$f.dump" "$f.log" "$f.check" "$f.diff"
    echo "seed $s: ok"
  done
  echo "fuzz: $n programs, $bad with findings (kept in $dir)"
  [ "$bad" -eq 0 ]
}

case ${1:-} in
  selftest) shift; cmd_selftest "$@" ;;
  check) shift; cmd_check "$@" ;;
  fuzz) shift; cmd_fuzz "$@" ;;
  *) usage; [ $# -eq 0 ] ;;
esac
