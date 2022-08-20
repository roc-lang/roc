#!/usr/bin/env bash

# Runs on every Netlify build, to set up the Netlify server.

set -euxo pipefail

rustup update
rustup default stable

ZIG_DIRNAME="zig-linux-x86_64-0.10.0-dev.3659+e5e6eb983"
wget https://ziglang.org/builds/zig-linux-x86_64-0.10.0-dev.3659+e5e6eb983.tar.xz
tar --extract --xz --file=${ZIG_DIRNAME}.tar.xz
PATH="$(pwd)/${ZIG_DIRNAME}:${PATH}"

export PATH
bash build.sh
