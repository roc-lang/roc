#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

git clone https://github.com/roc-lang/basic-cli.git

# fetch releases data and save to file
curl https://api.github.com/repos/roc-lang/roc/releases > roc_releases.json

# Get the url of the latest release. We're not using the latest main source code for easier reproducibility.
RELEASE_URL=$(./ci/get_latest_release_url.sh $1)

# get the archive from the url
mkdir roc_nightly && cd roc_nightly && curl -OL $RELEASE_URL

# decompress the tar
ls | grep "roc_nightly.*tar\.gz" | xargs tar -xzvf

# build the basic cli platform
./roc build ../basic-cli/examples/file.roc

cd ..
