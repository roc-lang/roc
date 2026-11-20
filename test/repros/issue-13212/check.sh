#!/bin/sh
# https://github.com/roc-lang/roc/issues/13212
set -eu

roc=${ROC:-roc}
probe=$(dirname "$0")/PositionProbe.roc
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# In this single-line input, the second '<' is at byte offset 21.
expected='[{ column: 1, line: 1 }, { column: 22, line: 1 }]'
if [ "$#" -eq 0 ]; then
    set -- --opt=speed
fi
"$roc" build "$@" "$probe" --output="$work/position-probe"
actual=$("$work/position-probe")
if [ "$actual" != "$expected" ]; then
    printf 'Expected: %s\nActual:   %s\n' "$expected" "$actual" >&2
    exit 1
fi
printf '%s\n' "$actual"
