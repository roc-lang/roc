#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

mkdir -p $HOME/.cargo
echo -e "[build]\nrustflags = [\"-C\", \"link-arg=-fuse-ld=lld\", \"-C\", \"target-cpu=native\"]" > $HOME/.cargo/config

ln -s /usr/bin/lld-8 /usr/local/bin/ld.lld
