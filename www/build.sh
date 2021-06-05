#!/bin/bash

set -euxo pipefail

rm -rf build/
cp -r public/ build/

pushd ..
echo 'Generating docs...'
cargo run docs compiler/builtins/docs/Bool.roc
mv generated-docs/ www/build/builtins
popd
