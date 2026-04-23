#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "${script_dir}/.." && pwd)"

cd "${repo_root}"

if [[ "${1:-}" == "--" ]]; then
  shift
fi

if [[ "$#" -eq 0 ]]; then
  echo "usage: ci/guarded_zig.sh <command> [args...]" >&2
  echo "example: ci/guarded_zig.sh zig build test-eval" >&2
  exit 2
fi

run_check() {
  printf '\n==> '
  printf '%q ' "$@"
  printf '\n'
  "$@"
}

run_perl_checks() {
  local found=0

  while IFS= read -r perl_check; do
    [[ -n "${perl_check}" ]] || continue
    found=1
    run_check perl "${perl_check}"
  done < <(find ci -maxdepth 1 -type f -name '*.pl' -print | LC_ALL=C sort)

  if [[ "${found}" -eq 0 ]]; then
    echo "error: no Perl checks found under ci/" >&2
    exit 1
  fi
}

run_perl_checks
run_check ci/check_ownership_boundaries.py
run_check bash ci/check_debug_vars.sh
run_check zig build check-fmt
run_check zig run ci/zig_lints.zig
run_check zig run ci/tidy.zig
run_check zig run ci/check_test_wiring.zig

printf '\n==> '
printf '%q ' "$@"
printf '\n'
exec "$@"
