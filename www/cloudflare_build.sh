#!/bin/bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# Download latest Roc nightly release
curl -fOL https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-linux_x86_64-latest.tar.gz

# rename nightly tar
mv $(ls | grep "roc_nightly.*tar\.gz") roc_nightly.tar.gz

# decompress the tar
tar -xzf roc_nightly.tar.gz

rm roc_nightly.tar.gz

# simplify nightly folder name
mv roc_nightly* roc_nightly

./roc_nightly/roc version

export PATH=$PATH:$(pwd)/roc_nightly

cd www
roc check build_website.roc
roc build build_website.roc
./build_website