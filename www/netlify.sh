#!/bin/bash

# Runs on every Netlify build, to set up the Netlify server.

set -euxo pipefail

rustup update
rustup default stable
rustup target add wasm32-unknown-unknown wasm32-wasi

ZIG_DIR="zig-linux-x86_64-0.9.1"
wget https://ziglang.org/download/0.9.1/${ZIG_DIR}.tar.xz

uname -a

xz --decompress ${ZIG_DIR}.tar.xz
tar xvf ${ZIG_DIR}.tar
export PATH="${ZIG_DIR}:${PATH}"

bash build.sh
