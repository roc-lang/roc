#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# set the working directory to the directory of the script
cd "$(dirname "$0")"

cargo build -p rust-platform

cp ../../../target/debug/librustplatform.a ./libhost.a
