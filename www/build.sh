#!/usr/bin/env bash

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
wget https://github.com/roc-lang/roc/archive/www.tar.gz

# Download the latest pre-built Web REPL as a zip file. (Build takes longer than Netlify's timeout.)
# TODO: When roc repo is public, download it from nightly builds.
wget https://github.com/brian-carroll/mock-repl/archive/refs/heads/deploy.zip
unzip deploy.zip
# Explicitly list the filenames, failing the build if they're not found
mv mock-repl-deploy/roc_repl_wasm.js repl/
mv mock-repl-deploy/roc_repl_wasm_bg.wasm repl/
rmdir mock-repl-deploy
rm deploy.zip

popd

pushd ..
echo 'Generating docs...'
cargo --version
rustc --version

# We set ROC_DOCS_ROOT_DIR=builtins so that links will be generated relative to
# "/builtins/" rather than "/" - which is what we want based on how the server
# is set up to serve them.
export ROC_DOCS_URL_ROOT=/builtins

cargo run --bin roc-docs crates/compiler/builtins/roc/*.roc
mv generated-docs/*.* www/build # move all the .js, .css, etc. files to build/
mv generated-docs/ www/build/builtins # move all the folders to build/builtins/

popd
