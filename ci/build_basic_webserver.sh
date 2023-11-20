#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

git clone https://github.com/roc-lang/basic-webserver.git

OS=$(uname -s)
ARCH=$(uname -m)

if [ "$OS" == "Linux" ]; then

    # check if musl-tools is installed
    if ! dpkg -l | grep -q musl-tools; then
        # install musl-tools with timeout for sudo problems with CI
        timeout 300s sudo apt-get install -y musl-tools
    fi
    
    cd basic-webserver/platform # we cd to install the target for the right rust version
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

cd roc_nightly

# prevent https://github.com/roc-lang/basic-webserver/issues/9
if [ "$OS" != "Linux" ] || [ "$ARCH" != "x86_64" ]; then
    # build the basic-webserver platform
    ./roc build ../basic-webserver/examples/echo.roc
fi

# We need this extra variable so we can safely check if $2 is empty later
EXTRA_ARGS=${2:-}

# In some rare cases it's nice to be able to use the legacy linker, so we produce the .o file to be able to do that
if [ -n "${EXTRA_ARGS}" ];
 then ./roc build $EXTRA_ARGS ../basic-webserver/examples/echo.roc
fi

cd ..
