#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# this makes the binaries a lot smaller
strip ./target/release-with-lto/roc
strip ./target/release-with-lto/roc_language_server

# to be able to delete "target" later
cp target/release-with-lto/roc ./roc
cp target/release-with-lto/roc_language_server ./roc_language_server

# delete unnecessary files and folders
git clean -fdx --exclude roc --exclude roc_language_server

mkdir $1


mv roc roc_language_server LICENSE LEGAL_DETAILS $1

mkdir $1/examples
mv examples/helloWorld.roc examples/platform-switching examples/cli $1/examples

mkdir -p $1/crates/compiler/builtins/bitcode
mv crates/roc_std $1/crates
mv crates/compiler/builtins/bitcode/src $1/crates/compiler/builtins/bitcode
 
tar -czvf "$1.tar.gz" $1