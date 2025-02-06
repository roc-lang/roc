#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# this makes the binaries a lot smaller
strip ./target/release-with-lto/roc
strip ./target/release-with-lto/roc_language_server

mkdir -p $1 $1/examples

mv target/release-with-lto/{roc,roc_language_server,lib} $1
mv LICENSE LEGAL_DETAILS $1

mv crates/cli/tests/platform-switching $1/examples
mv examples/README.md $1/examples

# temporary github.com/roc-lang/roc/pull/7231
rm $1/examples/platform-switching/roc_loves_rust.roc
rm -rf $1/examples/platform-switching/rust-platform

# copy zig builtins
if [ ! -d "$1/examples/platform-switching/zig-platform/glue" ]; then
    mkdir $1/examples/platform-switching/zig-platform/glue
    mv crates/compiler/builtins/bitcode/src/* $1/examples/platform-switching/zig-platform/glue
fi
 
tar -czvf "$1.tar.gz" $1
