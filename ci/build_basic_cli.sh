#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

git clone https://github.com/roc-lang/basic-cli.git

# build the basic cli platform
./target/release/roc build ../basic-cli/examples/file.roc

# We need this extra variable so we can safely check if $2 is empty later
EXTRA_ARGS=${2:-}

# In some rare cases it's nice to be able to use the legacy linker, so we produce the .o file to be able to do that
if [ -n "${EXTRA_ARGS}" ];
 then ./roc build $EXTRA_ARGS ../basic-cli/examples/file.roc
fi

cd ..
