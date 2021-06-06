#!/bin/bash

set -euxo pipefail

# cd into the directory where this script lives.
# This allows us to run this script from the root project directory,
# which is what Netlify wants to do.
SCRIPT_RELATIVE_DIR=$(dirname "${BASH_SOURCE[0]}")
cd $SCRIPT_RELATIVE_DIR

rm -rf build/
cp -r public/ build/

pushd ..
echo 'Generating docs...'
cargo run docs compiler/builtins/docs/Bool.roc
mv generated-docs/ www/build/builtins
popd
