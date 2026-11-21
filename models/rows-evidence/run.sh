#!/usr/bin/env bash
# Entry point for the rows and evidence model. Run ./run.sh with no arguments for usage.
set -euo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROC=${ROC:-$(git -C "$HERE" rev-parse --show-toplevel)/zig-out/bin/roc}
OUT=$HERE/out
MODEL=$HERE/lean/.lake/build/bin/rows-evidence
LIMIT=300 # seconds per compiler run
export ROC MODEL LIMIT

usage() {
  cat <<'EOF'
usage: ./run.sh <command>

  selftest                 calibrate the model, check the fixtures in tests/, run three generated programs
  check <file>...          .roc: compile with ROC_LIFTED_IR_EXPORT and check every Lifted IR export
                           .lifted: check that export
  fuzz [N] [--seed S]      generate N programs (default 20); compare the model's verdict with roc check,
                           roc test under both --specialize strategies, and the Lifted IR checker
  mutate [N] <file.lifted>...
                           mutation-test the Lifted IR checker on N single-id mutants (default 100)

ROC selects the compiler (default: zig-out/bin/roc at the repository root). Output goes to out/.
EOF
}

build() {
  (cd "$HERE/lean" && lake build -q)
}

need_roc() {
  [[ -x $ROC ]] || { echo "no roc at $ROC (build it with zig build roc, or set ROC)" >&2; exit 1; }
}

# compile_one DIR: run roc on DIR/H.roc and write DIR/verdict, one line:
#   <name> <check> <test-yes> <test-no> <lifted>
# check is OK, REJECT or PANIC; a test is OK, FAIL, CRASH or PANIC (- when check did not pass);
# lifted is OK, VIOL or - (no export, because ROC does not have ROC_LIFTED_IR_EXPORT).
compile_one() {
  local dir=$1 name check yes=- no=- lifted=-
  name=$(basename "$dir")
  rm -rf "$dir/lifted"
  mkdir -p "$dir/lifted"
  panicked() { grep -q -E "panic|invariant violated|compiler bug|reached unreachable|Segmentation fault" "$1"; }
  local rc=0
  (cd "$dir" && timeout "$LIMIT" "$ROC" check --no-cache H.roc) >"$dir/check.log" 2>&1 || rc=$?
  if panicked "$dir/check.log" || ((rc >= 124)); then
    check=PANIC
  elif grep -q -E "── [1-9][0-9]* errors? and" "$dir/check.log"; then
    check=REJECT
  else
    check=OK
    local s
    for s in yes no; do
      rc=0
      if [[ $s == yes ]]; then
        (cd "$dir" && ROC_LIFTED_IR_EXPORT="$dir/lifted" timeout "$LIMIT" "$ROC" test --no-cache --specialize=yes H.roc) \
          >"$dir/test-$s.log" 2>&1 || rc=$?
      else
        (cd "$dir" && timeout "$LIMIT" "$ROC" test --no-cache --specialize=no H.roc) >"$dir/test-$s.log" 2>&1 || rc=$?
      fi
      local r
      if panicked "$dir/test-$s.log" || ((rc >= 124)); then r=PANIC
      elif grep -q -E "never exist|Roc crashed" "$dir/test-$s.log"; then r=CRASH
      elif grep -q -E "^All \([0-9]+\) tests passed" "$dir/test-$s.log"; then r=OK
      else r=FAIL; fi
      if [[ $s == yes ]]; then yes=$r; else no=$r; fi
    done
    if compgen -G "$dir/lifted/*.lifted" >/dev/null; then
      if "$MODEL" lifted "$dir"/lifted/*.lifted >"$dir/lifted.log"; then lifted=OK; else lifted=VIOL; fi
    fi
  fi
  printf '%s\t%s\t%s\t%s\t%s\n' "$name" "$check" "$yes" "$no" "$lifted" >"$dir/verdict"
}
export -f compile_one

# run_programs DIR SEED N: generate, compile and compare; prints the table and writes DIR/summary.tsv.
# Returns nonzero when the model and the compiler disagree on any program.
run_programs() {
  local dir=$1 seed=$2 n=$3
  rm -rf "$dir"
  mkdir -p "$dir"
  "$MODEL" gen "$seed" "$n" "$dir"
  "$MODEL" model "$seed" "$n" >"$dir/model.tsv"
  ls -d "$dir"/g*/ | sed 's:/$::' | xargs -P "${JOBS:-6}" -I{} bash -c 'compile_one "$1"' _ {}
  # join: name model violations check test-yes test-no lifted
  local bad=0 line name verdict nviol check yes no lifted agree
  : >"$dir/summary.tsv"
  while IFS=$'\t' read -r name verdict nviol _; do
    [[ $verdict == ACCEPT ]] || nviol=-
    IFS=$'\t' read -r _ check yes no lifted <"$dir/$name/verdict"
    if [[ $verdict == ACCEPT && $nviol == 0 && $check == OK && $yes == OK && $no == OK && $lifted != VIOL ]]; then
      agree=agree
    else
      agree=DISAGREE
      bad=$((bad + 1))
    fi
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$verdict" "$nviol" "$check" "$yes" "$no" "$lifted" "$agree" >>"$dir/summary.tsv"
  done < <(sort -V "$dir/model.tsv")
  printf 'program\tmodel\tviolations\tcheck\ttest-yes\ttest-no\tlifted\n'
  grep DISAGREE "$dir/summary.tsv" || true
  echo "$(wc -l <"$dir/summary.tsv" | tr -d ' ') programs, $bad disagreements; details in $dir"
  ((bad == 0))
}

selftest() {
  need_roc
  build
  echo "== calibration"
  "$MODEL" calibrate
  echo "== Lifted IR fixtures"
  "$MODEL" lifted "$HERE/tests/all-kinds.lifted"
  local log=$OUT/selftest/call-result-is-backing.log
  mkdir -p "$OUT/selftest"
  if "$MODEL" lifted "$HERE/tests/call-result-is-backing.lifted" >"$log"; then
    echo "FAIL: the checker accepted tests/call-result-is-backing.lifted" >&2; exit 1
  fi
  grep -q "T-CallProc" "$log" || { echo "FAIL: expected a T-CallProc violation" >&2; cat "$log" >&2; exit 1; }
  echo "ok   tests/call-result-is-backing.lifted is rejected by T-CallProc"
  echo "== generated programs"
  # The generator, printer and evaluator must produce programs that roc accepts and whose expects
  # pass. A Lifted IR violation here is a compiler finding (for example #13264 on a compiler
  # without its fix), so it is shown but does not fail the selftest.
  run_programs "$OUT/selftest/gen" 100 3 || true
  if awk -F'\t' '$2 != "ACCEPT" || $3 != "0" || $4 != "OK" || $5 != "OK" || $6 != "OK" { bad = 1 } END { exit !bad }' \
    "$OUT/selftest/gen/summary.tsv"; then
    echo "FAIL: a generated program was not accepted by both the model and the compiler" >&2; exit 1
  fi
  if grep -q VIOL "$OUT/selftest/gen/summary.tsv"; then
    echo "note: the Lifted IR violations above are compiler findings, not selftest failures"
  fi
  echo "selftest passed"
}

check() {
  (($# > 0)) || { usage >&2; exit 1; }
  need_roc
  build
  local f bad=0
  for f in "$@"; do
    case $f in
      *.lifted)
        "$MODEL" lifted "$f" || bad=1
        ;;
      *.roc)
        local abs dir rc=0
        abs=$(cd "$(dirname "$f")" && pwd)/$(basename "$f")
        dir=$OUT/check/$(basename "$f" .roc)
        rm -rf "$dir"
        mkdir -p "$dir"
        if grep -v -E '^[[:space:]]*(#|$)' "$abs" | head -1 | grep -q -E '^app\b'; then
          (cd "$(dirname "$abs")" && ROC_LIFTED_IR_EXPORT="$dir" timeout "$LIMIT" "$ROC" build --no-cache --opt=dev \
            --specialize=yes --output="$dir/app" "$abs") >"$dir/roc.log" 2>&1 || rc=$?
        else
          (cd "$(dirname "$abs")" && ROC_LIFTED_IR_EXPORT="$dir" timeout "$LIMIT" "$ROC" test --no-cache \
            --specialize=yes "$abs") >"$dir/roc.log" 2>&1 || rc=$?
        fi
        echo "$f: roc exited $rc (log: $dir/roc.log)"
        if ! compgen -G "$dir/*.lifted" >/dev/null; then
          echo "$f: no Lifted IR export. Either roc failed before lifting (see the log), or ROC is not" \
            "a Debug compiler with ROC_LIFTED_IR_EXPORT" >&2
          bad=1
          continue
        fi
        "$MODEL" lifted "$dir"/*.lifted || bad=1
        ;;
      *)
        echo "$f: expected a .roc or .lifted file" >&2
        bad=1
        ;;
    esac
  done
  return $bad
}

fuzz() {
  local n=20 seed=$((RANDOM * 32768 + RANDOM))
  while (($# > 0)); do
    case $1 in
      --seed) seed=$2; shift 2 ;;
      *) n=$1; shift ;;
    esac
  done
  need_roc
  build
  echo "fuzz: $n programs from seed $seed"
  run_programs "$OUT/fuzz/$seed" "$seed" "$n"
}

mutate() {
  local n=100
  if [[ ${1:-} =~ ^[0-9]+$ ]]; then n=$1; shift; fi
  (($# > 0)) || { usage >&2; exit 1; }
  build
  python3 "$HERE/mutate_lifted.py" "$MODEL" "$OUT/mutate" "$n" 7 "$@"
}

case ${1:-} in
  selftest) shift; selftest "$@" ;;
  check) shift; check "$@" ;;
  fuzz) shift; fuzz "$@" ;;
  mutate) shift; mutate "$@" ;;
  *) usage ;;
esac
