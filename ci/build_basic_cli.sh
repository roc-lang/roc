#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

git clone https://github.com/roc-lang/basic-cli.git

cd basic-cli
git checkout new-release
cd ..

if [ "$(uname -m)" == "x86_64" ] && [ "$(uname -s)" == "Linux" ]; then
    sudo apt-get install musl-tools
    cd basic-cli/src # we cd to install the target for the right rust version
    rustup target add x86_64-unknown-linux-musl 
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

# build the basic cli platform
./roc build ../basic-cli/examples/countdown.roc

# We need this extra variable so we can safely check if $2 is empty later
EXTRA_ARGS=${2:-}

# In some rare cases it's nice to be able to use the legacy linker, so we produce the .o file to be able to do that
if [ -n "${EXTRA_ARGS}" ];
 then ./roc build $EXTRA_ARGS ../basic-cli/examples/countdown.roc
fi

cd ..
