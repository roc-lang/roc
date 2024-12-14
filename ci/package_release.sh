#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# this makes the binaries a lot smaller
strip ./target/release-with-lto/roc
strip ./target/release-with-lto/roc_language_server

mkdir -p $1 $1/examples $1/crates/compiler/builtins/bitcode

mv target/release-with-lto/{roc,roc_language_server,lib} $1
mv LICENSE LEGAL_DETAILS $1

mv examples/platform-switching $1/examples

mv crates/roc_std $1/crates
mv crates/compiler/builtins/bitcode/src $1/crates/compiler/builtins/bitcode
 
tar -czvf "$1.tar.gz" $1
