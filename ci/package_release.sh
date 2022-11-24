#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

cp target/release/roc ./roc # to be able to exclude "target" later in the tar command
tar -czvf $1 --exclude="target" --exclude="zig-cache" roc LICENSE LEGAL_DETAILS examples/helloWorld.roc examples/platform-switching examples/cli crates/roc_std
