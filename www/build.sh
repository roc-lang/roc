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
REPL_TARFILE="roc_repl_wasm.tar.gz"
wget https://github.com/roc-lang/roc/releases/download/nightly/$REPL_TARFILE
tar -xzf $REPL_TARFILE -C repl
rm $REPL_TARFILE
ls -lh repl

popd

pushd ..
echo 'Generating builtin docs...'
cargo --version
rustc --version

# We set ROC_DOCS_ROOT_DIR=builtins so that links will be generated relative to
# "/builtins/" rather than "/" - which is what we want based on how the server
# is set up to serve them.
export ROC_DOCS_URL_ROOT=/builtins

cargo run --bin roc-docs crates/compiler/builtins/roc/*.roc
mv generated-docs/*.* www/build # move all the .js, .css, etc. files to build/
mv generated-docs/ www/build/builtins # move all the folders to build/builtins/

echo 'Generating CLI example platform docs...'
# Change ROC_DOCS_ROOT_DIR=builtins so that links will be generated relative to
# "/examples/cli/" rather than "/builtins/"
export ROC_DOCS_URL_ROOT=/examples/cli

# Until https://github.com/roc-lang/roc/issues/3280 is done,
# manually exclude the Internal* modules and `main.roc`.
ls examples/cli/cli-platform/*.roc | grep -v Internal | grep -v main.roc | grep -v Effect.roc | xargs cargo run --bin roc-docs

mkdir www/build/examples
rm generated-docs/*.* # we already copied over the *.js and *.css files earlier, so just drop these.
mv generated-docs/ www/build/examples/cli # move all the folders to build/examples/cli

popd
