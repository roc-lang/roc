#!/bin/bash

set -euxo pipefail

# cd into the directory where this script lives.
# This allows us to run this script from the root project directory,
# which is what Netlify wants to do.
SCRIPT_RELATIVE_DIR=$(dirname "${BASH_SOURCE[0]}")
cd $SCRIPT_RELATIVE_DIR

rm -rf build/
cp -r public/ build/

pushd build

# grab the source code and copy it to Netlify's server; if it's not there, fail the build.
wget https://github.com/rtfeldman/elm-css/files/8037422/roc-source-code.zip

# Copy REPL webpage source files
cp -r ../../repl_www/public/* .

# grab the pre-compiled REPL and copy it to Netlify's server; if it's not there, fail the build.
wget https://github.com/brian-carroll/mock-repl/archive/refs/heads/deploy.zip
unzip deploy.zip
mv mock-repl-deploy/* .
rmdir mock-repl-deploy
rm deploy.zip

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

# These just need to be defined so that some env! macros don't fail.
BUILTINS_WASM32_O=""
BUILTINS_HOST_O=""

# We run the CLI with --no-default-features because that way we don't have the
# "llvm" feature and therefore don't depend on LLVM being installed on the
# system. (Netlify's build servers have Rust installed, but not LLVM.)
cargo run --bin roc-docs compiler/builtins/roc/*.roc
mv generated-docs/*.* www/build # move all the .js, .css, etc. files to build/
mv generated-docs/ www/build/builtins # move all the folders to build/builtins/
popd
