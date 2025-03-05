#!/usr/bin/env bash

# Runs on every Netlify build, to set up the Netlify server.

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

rustup update
# Get Rust version from rust-toolchain.toml
RUST_VERSION=$(grep "channel =" /opt/buildhome/repo/rust-toolchain.toml | head -1 | cut -d '"' -f 2)
rustup default "$RUST_VERSION"

bash build.sh
