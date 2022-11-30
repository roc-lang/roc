#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# cd into the directory where this script lives.
# This allows us to run this script from the root project directory,
# which is what Netlify wants to do.
SCRIPT_RELATIVE_DIR=$(dirname "${BASH_SOURCE[0]}")
cd $SCRIPT_RELATIVE_DIR

rm -rf build/
cp -r public/ build/

# download fonts just-in-time so we don't have to bloat the repo with them.
DESIGN_ASSETS_COMMIT="4d949642ebc56ca455cf270b288382788bce5873"
DESIGN_ASSETS_TARFILE="roc-lang-design-assets-4d94964.tar.gz"
DESIGN_ASSETS_DIR="roc-lang-design-assets-4d94964"

wget -O $DESIGN_ASSETS_TARFILE https://github.com/roc-lang/design-assets/tarball/$DESIGN_ASSETS_COMMIT
tar -xzf $DESIGN_ASSETS_TARFILE
mv $DESIGN_ASSETS_DIR/fonts build/
rm -rf $DESIGN_ASSETS_TARFILE $DESIGN_ASSETS_DIR

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

# Manually add this tip to all the builtin docs.
find www/build/builtins -type f -name 'index.html' -exec sed -i 's!</nav>!<div class="builtins-tip"><b>Tip:</b> <a href="/different-names">Some names</a> differ from other languages.</div></nav>!' {} \;

echo 'Generating CLI example platform docs...'
# Change ROC_DOCS_ROOT_DIR=builtins so that links will be generated relative to
# "/packages/basic-cli/" rather than "/builtins/"
export ROC_DOCS_URL_ROOT=/packages/basic-cli

rm -rf ./downloaded-basic-cli

git clone --depth 1 https://github.com/roc-lang/basic-cli.git downloaded-basic-cli

# Until https://github.com/roc-lang/roc/issues/3280 is done,
# manually exclude the Internal* modules and `main.roc`.
ls downloaded-basic-cli/src/*.roc | grep -v Internal | grep -v main.roc | grep -v Effect.roc | xargs cargo run --bin roc-docs

rm -rf ./downloaded-basic-cli

BASIC_CLI_PACKAGE_DIR="www/build/packages/basic-cli"
mkdir -p $BASIC_CLI_PACKAGE_DIR
rm generated-docs/*.* # we already copied over the *.js and *.css files earlier, so just drop these.
mv generated-docs/* $BASIC_CLI_PACKAGE_DIR # move all the folders to build/packages/basic-cli

popd
