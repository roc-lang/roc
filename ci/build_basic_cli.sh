#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

git clone https://github.com/roc-lang/basic-cli.git

# Get the url of the latest release. We're not using the latest main source code for easier reproducibility.
RELEASE_URL=$(./ci/get_latest_release_url.sh $1)

# get the archive from the url
curl -OL $RELEASE_URL

# decompress the tar
ls | grep "roc_nightly.*tar\.gz" | xargs tar -xzvf

# delete tar
ls | grep "roc_nightly.*tar\.gz" | xargs rm -rf

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
