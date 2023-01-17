#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

cp target/release/roc ./roc # to be able to delete "target" later

# delete unnecessary files and folders
find . -name "target" -depth -type d -exec rm -rf {} \;
find . -name "zig-cache" -depth -type d -exec rm -rf {} \;

find . -name "*.o" -exec rm -rf {} \;
find . -name "*.rh1" -exec rm -rf {} \;
find . -name "*.rm1" -exec rm -rf {} \;

fd --type executable . './examples' -X rm

mkdir $1
tar -czvf "$1.tar.gz" roc LICENSE LEGAL_DETAILS examples/helloWorld.roc examples/platform-switching examples/cli crates/roc_std crates/compiler/builtins/bitcode/src
