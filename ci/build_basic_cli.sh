#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

git clone https://github.com/roc-lang/basic-cli.git

# Get the url of the latest release. We're not using the latest main source code for easier reproducibility.
if [[ "$(uname -s)" == "Linux" ]]; then
    RELEASE_URL="https://github.com/roc-lang/roc/releases/download/nightly/temp_linux_rm2.tar.gz"
elif [[ "$(uname -s)" == "Darwin" ]]; then
    if [[ "$(uname -m)" == "x86_64" ]]; then
        RELEASE_URL="https://github.com/roc-lang/roc/releases/download/nightly/temp_macos_x86_64_rm2.tar.gz"
    else
        RELEASE_URL="https://github.com/roc-lang/roc/releases/download/nightly/temp_macos_arm64_rm2.tar.gz"
    fi
else
    echo "Unknown operating system: $(uname -s)"
fi

# get the archive from the url
mkdir roc_nightly && cd roc_nightly && curl -OL $RELEASE_URL

# decompress the tar
ls | grep "temp.*tar\.gz" | xargs tar -xzvf

# simplify dir name
mv roc_nightly* roc_nightly

# cd roc_nightly

# build the basic cli platform
./roc_nightly/roc build ../basic-cli/examples/file.roc

# We need this extra variable so we can safely check if $2 is empty later
EXTRA_ARGS=${2:-}

# In some rare cases it's nice to be able to use the legacy linker, so we produce the .o file to be able to do that
if [ -n "${EXTRA_ARGS}" ];
 then ./roc_nightly/roc build $EXTRA_ARGS ../basic-cli/examples/file.roc
fi

cd ..
