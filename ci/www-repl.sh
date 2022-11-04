#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

crates/repl_wasm/build-www.sh `pwd`/roc_repl_wasm.tar.gz
