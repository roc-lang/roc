#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

git clone https://github.com/roc-lang/basic-cli.git
cd basic-cli
git checkout $RELEASE_TAG
cd ..

if [ "$(uname -s)" == "Linux" ]; then

    # check if musl-tools is installed
    if ! dpkg -l | grep -q musl-tools; then
        # install musl-tools with timeout for sudo problems with CI
        timeout 300s sudo apt-get install -y musl-tools
    fi
    
    cd basic-cli/platform # we cd to install the target for the right rust version
    if [ "$(uname -m)" == "x86_64" ]; then
        rustup target add x86_64-unknown-linux-musl
    elif [ "$(uname -m)" == "aarch64" ]; then
        rustup target add aarch64-unknown-linux-musl
    fi
    cd ../..
fi

mv $(ls -d artifact/* | grep "roc_nightly.*tar\.gz" | grep "$1") ./roc_nightly.tar.gz

# decompress the tar
tar -xzvf roc_nightly.tar.gz

# delete tar
rm roc_nightly.tar.gz

# simplify dir name
mv roc_nightly* roc_nightly

cd roc_nightly
export PATH="$(pwd -P):$PATH"
cd ..

# temp test
roc version

cd basic-cli
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    if [[ $(uname -m) == "aarch64" ]]; then
        target_arch="aarch64-unknown-linux-musl"
    else
        target_arch="x86_64-unknown-linux-musl"
    fi
fi
./jump-start.sh

# build the basic cli platform
roc build.roc --prebuilt-platform

cd ..
