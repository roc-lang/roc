#!/usr/bin/env bash
# Pin the same-allocation fast path in string equality.
#
# When both sides of a `Str` comparison are the same allocation, equal byte
# pointers and equal lengths already prove the contents equal, so the answer is
# a pointer comparison and no byte of either string is read. That makes
# self-comparison cost the same for a 4 KB string as for a 68 KB one.
#
# test/cli/str_eq_same_allocation.roc compares one runtime-built string against
# itself a fixed number of times, with the length taken from the command line.
# This runs that binary under Valgrind twice, once with a short length control
# and once with a seventeen-times-longer one, and fails if the instruction count
# grew with the string. Instruction counts are exact and repeatable, so this
# measures the fast path rather than the machine it runs on.
#
# Comparing two runs of one binary is deliberate: it pins the property (the work
# does not scale with the string) instead of an absolute number that would have
# to be updated whenever anything else in the fixture's code generation moved.
set -euo pipefail

if [ "$(uname -s)" != "Linux" ]; then
    echo "same-allocation string equality check needs Valgrind, which is Linux-only here"
    exit 0
fi

if ! command -v valgrind >/dev/null 2>&1; then
    echo "valgrind is required for the same-allocation string equality check" >&2
    exit 1
fi

roc_bin="$1"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
fixture="$repo_root/test/cli/str_eq_same_allocation.roc"
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "$tmp_dir"' EXIT

cd "$repo_root"

# Every byte of the control argument stands for 4096 bytes of string, so these
# two runs compare a 4 KB string against itself and a 68 KB one.
short_control="x"
long_control="xxxxxxxxxxxxxxxxx"

# The two runs still differ by the cost of building the longer string and of
# handing the longer argument to the app, which is tens of thousands of
# instructions. Reading the bytes on every comparison instead costs hundreds of
# millions, so anything in between is unambiguous.
max_growth=4000000

exe="$tmp_dir/str-eq-same-allocation"
"$roc_bin" build --opt=speed --no-cache --output="$exe" "$fixture" >/dev/null

count_instructions() {
    local control="$1"
    local log="$tmp_dir/callgrind-${#control}.log"

    if ! valgrind --tool=callgrind --callgrind-out-file=/dev/null "$exe" "$control" >/dev/null 2>"$log"; then
        echo "the fixture failed with a ${#control}-byte control argument" >&2
        cat "$log" >&2
        return 1
    fi

    local collected
    collected="$(sed -n 's/^==[0-9]*== Collected *: *\([0-9][0-9]*\).*$/\1/p' "$log" | head -n 1)"
    if [ -z "$collected" ]; then
        echo "no instruction count in Valgrind's output for a ${#control}-byte control argument" >&2
        cat "$log" >&2
        return 1
    fi

    echo "$collected"
}

short_count="$(count_instructions "$short_control")" || exit 1
long_count="$(count_instructions "$long_control")" || exit 1
growth=$((long_count - short_count))

if [ "$growth" -le "$max_growth" ]; then
    echo "same-allocation string equality: ${short_count} instructions at 4 KB, ${long_count} at 68 KB (grew by ${growth})"
    exit 0
fi

cat >&2 <<EOF
same-allocation string equality reads the string's bytes.

Comparing a 4 KB string against itself took ${short_count} instructions and
comparing a 68 KB one took ${long_count}, a growth of ${growth} against a budget
of ${max_growth}. Self-comparison that scales with the string is comparing the
contents byte by byte, which means the equal-pointer short circuit is missing
from the code generated at --opt=speed.

To see the work, build and time the fixture directly:

    roc build --opt=speed --no-cache --output=/tmp/str-eq \\
        test/cli/str_eq_same_allocation.roc
    /tmp/str-eq x
    /tmp/str-eq xxxxxxxxxxxxxxxxx
EOF
exit 1
