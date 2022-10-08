#!/usr/bin/env bash
cp target/release/roc ./roc # to be able to exclude "target" later in the tar command
cp -r target/release/lib ./lib
tar -czvf $1 --exclude="target" --exclude="zig-cache" roc lib LICENSE LEGAL_DETAILS examples/helloWorld.roc examples/cli crates/roc_std
