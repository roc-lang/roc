#!/bin/bash

# Runs on every Netlify build, to set up the Netlify server.

set -euxo pipefail

rustup update
rustup default stable

ZIG_DIRNAME="zig-linux-x86_64-0.9.1"
wget https://ziglang.org/download/0.9.1/${ZIG_DIRNAME}.tar.xz
tar --extract --xz --file=${ZIG_DIRNAME}.tar.xz
PATH="$(pwd)/${ZIG_DIRNAME}:${PATH}"

export PATH
bash build.sh
