#!/bin/bash

# Runs on every Netlify build, to set up the Netlify server.

set -euxo pipefail

mkdir $HOME/zig
pushd $HOME/zig

curl -L https://ziglang.org/download/0.8.1/zig-linux-x86_64-0.8.1.tar.xz | tar -xJ --strip-components=1 -C .

export PATH=$PATH:$HOME/zig
popd

rustup update
rustup default stable

bash build.sh
