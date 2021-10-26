#!/bin/bash

RUST_VERSION=1.54

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > rustup.sh
chmod +x rustup.sh
./rustup.sh -y
source $HOME/.cargo/env
rustup install $RUST_VERSION
rustup override set $RUST_VERSION