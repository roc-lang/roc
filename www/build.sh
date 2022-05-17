#!/bin/bash

set -euxo pipefail

# cd into the directory where this script lives.
# This allows us to run this script from the root project directory,
# which is what Netlify wants to do.
SCRIPT_RELATIVE_DIR=$(dirname "${BASH_SOURCE[0]}")
cd $SCRIPT_RELATIVE_DIR

rm -rf build/
cp -r public/ build/

# grab the source code and copy it to Netlify's server; if it's not there, fail the build.
pushd build
wget https://github.com/rtfeldman/elm-css/files/8037422/roc-source-code.zip
popd

pushd ..
echo 'Generating docs...'
cargo --version
rustc --version

# We set RUSTFLAGS to -Awarnings to ignore warnings during this build,
# because when building without "the" llvm feature (which is only ever done
# for this exact use case), the result is lots of "unused" warnings!
RUSTFLAGS=-Awarnings

# We set ROC_DOCS_ROOT_DIR=builtins so that links will be generated relative to
# "/builtins/" rather than "/" - which is what we want based on how the server
# is set up to serve them.
export ROC_DOCS_URL_ROOT=/builtins

cargo run --bin roc-docs compiler/builtins/roc/*.roc
mv generated-docs/*.* www/build # move all the .js, .css, etc. files to build/
mv generated-docs/ www/build/builtins # move all the folders to build/builtins/

echo "Building Web REPL..."
repl_www/build.sh www/build

echo "Asset sizes:"
ls -lh www/build/*

popd
