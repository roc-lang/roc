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

# pushd ..
# echo 'Generating docs...'
# cargo --version
# rustc --version
# # We run the CLI with --no-default-features because that way we don't have the
# # "llvm" feature and therefore don't depend on LLVM being installed on the
# # system. (Netlify's build servers have Rust installed, but not LLVM.)
# #
# # We set RUSTFLAGS to -Awarnings to ignore warnings during this build,
# # because when building without "the" llvm feature (which is only ever done
# # for this exact use case), the result is lots of "unused" warnings!
# #
# # We set ROC_DOCS_ROOT_DIR=builtins so that links will be generated relative to
# # "/builtins/" rather than "/" - which is what we want based on how the server
# # is set up to serve them.
# RUSTFLAGS=-Awarnings ROC_DOCS_URL_ROOT=builtins cargo run -p roc_cli --no-default-features docs compiler/builtins/docs/*.roc
# mv generated-docs/ www/build/builtins
# popd
