#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

git clone https://github.com/roc-lang/basic-webserver.git
cd basic-webserver
git checkout $RELEASE_TAG
cd ..

OS=$(uname -s)
ARCH=$(uname -m)

if [ "$OS" == "Linux" ]; then

    # check if musl-tools is installed
    if ! dpkg -l | grep -q musl-tools; then
        # install musl-tools with timeout for sudo problems with CI
        timeout 300s sudo apt-get install -y musl-tools
    fi
    
    cd basic-webserver/platform # we cd to install the target for the right rust version

    # Remove these functions that don't work with musl.
    sed -i.bak -e '/time_accessed!,$/d' -e '/time_modified!,$/d' -e '/time_created!,$/d' -e '/^time_accessed!/,/^$/d' -e '/^time_modified!/,/^$/d' -e '/^time_created!/,/^$/d' -e '/^import Utc exposing \[Utc\]$/d' File.roc
    # Remove sed backup file
    rm File.roc.bak
    
    if [ "$ARCH" == "x86_64" ]; then
        rustup target add x86_64-unknown-linux-musl
    elif [ "$ARCH" == "aarch64" ]; then
        rustup target add aarch64-unknown-linux-musl
    fi
    cd ../..
fi

# simplify tar name
mv $(ls -d artifact/* | grep "roc_nightly.*tar\.gz" | grep "$1") ./roc_nightly.tar.gz

# decompress the tar
tar -xzvf roc_nightly.tar.gz

# delete tar
rm roc_nightly.tar.gz

# simplify dir name
mv roc_nightly* roc_nightly

# add roc to PATH
cd roc_nightly
export PATH="$(pwd -P):$PATH"
cd ..

cd basic-webserver

roc build.roc

cd ..
