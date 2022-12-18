#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# print all found executable files in the dir examples 
fd --type executable . './examples'

# remove all executables from the dir examples
fd --type executable . './examples' -X rm

cp target/release/roc ./roc # to be able to exclude "target" later in the tar command
tar -czvf $1 --exclude="target" --exclude="zig-cache" --exclude='*.o' roc LICENSE LEGAL_DETAILS examples/helloWorld.roc examples/platform-switching examples/cli crates/roc_std crates/compiler/builtins/bitcode/src
