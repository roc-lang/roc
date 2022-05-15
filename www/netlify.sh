#!/bin/bash

# Runs on every Netlify build, to set up the Netlify server.

set -euxo pipefail

rustup update
rustup default stable
rustup target add wasm32-unknown-unknown wasm32-wasi

cd netlify-install

ZIG_DIRNAME="zig-linux-x86_64-0.9.1"
wget https://ziglang.org/download/0.9.1/${ZIG_DIRNAME}.tar.xz
tar --extract --xz --file=${ZIG_DIRNAME}.tar.xz

# Work around an issue with wasm-pack where it fails to install wasm-opt (from binaryen) on some CI systems
# https://github.com/rustwasm/wasm-pack/issues/864
BINARYEN_DIRNAME="binaryen-version_108-x86_64-linux"
wget https://github.com/WebAssembly/binaryen/releases/download/version_108/${BINARYEN_DIRNAME}.tar.gz
tar --extract --gzip --file=${BINARYEN_DIRNAME}.tar.gz

cd ..

export PATH="$(pwd)/netlify-install/${BINARYEN_DIRNAME}/bin:$(pwd)/netlify-install/${ZIG_DIRNAME}:${PATH}"
bash build.sh
