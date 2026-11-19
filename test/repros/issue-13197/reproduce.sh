#!/bin/sh
# https://github.com/roc-lang/roc/issues/13197
# Use the immutable roc-signals snapshot documented in README.md.
# Every build must succeed; a compiler crash fails this reproduction.
set -eu

roc=${1:?usage: sh reproduce.sh /path/to/roc /path/to/roc-signals /path/to/output-dir}
source=${2:?provide the pinned roc-signals source directory}
output=${3:?provide an output directory}
mkdir -p "$output"

"$roc" check --opt=speed "$source/fuzz/startup-placeholders/main.roc"
"$roc" build --opt=speed --timings "$source/fuzz/startup-placeholders/main.roc" \
    --output="$output/startup-placeholders"
"$roc" build --opt=speed --timings "$source/fuzz/wire/main.roc" \
    --output="$output/wire"
