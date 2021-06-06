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
# We run the CLI with --no-default-features because that way we don't have a LLVM
# dependency. (Netlify's build servers have Rust installed, but not LLVM.)
cargo run -p roc_cli --no-default-features docs compiler/builtins/docs/Bool.roc

mv generated-docs/ www/build/builtins

pushd www/build/builtins

for f in ./*.html; do
    DIRNAME=$(echo "$f" | sed s/.html//)
    mkdir "$DIRNAME"
    mv "$f" "$DIRNAME"/index.html
done

popd

popd
