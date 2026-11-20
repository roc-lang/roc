#!/bin/sh
# Run from the repository root after `zig build roc`. The default target
# requires an x86_64 macOS environment (native or Rosetta).
set -eu

target=${1:-x64mac}
roc=${ROC:-./zig-out/bin/roc}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

"$roc" build --no-cache --opt=dev --target="$target" \
    test/repros/issue-13221/Repro.roc --output="$tmp/repro"

basename=round-trip-search-on-the-timeline.scm
printf '%s\n%s\n' "$basename" "$basename" > "$tmp/expected"
"$tmp/repro" "prefix/$basename" "$basename" > "$tmp/actual"
diff -u "$tmp/expected" "$tmp/actual"
