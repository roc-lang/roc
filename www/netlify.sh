#!/usr/bin/env bash

# Runs on every Netlify build, to set up the Netlify server.

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

rustup update
rustup default stable

ZIG_DIRNAME="zig-linux-x86_64-0.11.0"
wget https://ziglang.org/download/0.11.0/${ZIG_DIRNAME}.tar.xz
tar --extract --xz --file=${ZIG_DIRNAME}.tar.xz
PATH="$(pwd)/${ZIG_DIRNAME}:${PATH}"

export PATH
bash build.sh
