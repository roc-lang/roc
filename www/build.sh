#!/usr/bin/env bash

# run from root of repo with `./www/build.sh`
# check www/README.md to run a test server

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# check if jq is installed
jq --version

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

curl -fLJO https://github.com/roc-lang/design-assets/tarball/$DESIGN_ASSETS_COMMIT  
tar -xzf $DESIGN_ASSETS_TARFILE
mv $DESIGN_ASSETS_DIR/fonts build/
rm -rf $DESIGN_ASSETS_TARFILE $DESIGN_ASSETS_DIR

# grab the source code and copy it to Netlify's server; if it's not there, fail the build.
pushd build
curl -fLJO https://github.com/roc-lang/roc/archive/www.tar.gz

# Download the latest pre-built Web REPL as a zip file. (Build takes longer than Netlify's timeout.)
REPL_TARFILE="roc_repl_wasm.tar.gz"
curl -fLJO https://github.com/roc-lang/roc/releases/download/nightly/$REPL_TARFILE
tar -xzf $REPL_TARFILE -C repl
rm $REPL_TARFILE
ls -lh repl

popd

pushd ..
echo 'Generating builtin docs...'
cargo --version

# We set ROC_DOCS_ROOT_DIR=builtins so that links will be generated relative to
# "/builtins/" rather than "/" - which is what we want based on how the server
# is set up to serve them.
export ROC_DOCS_URL_ROOT=/builtins

cargo run --release --bin roc-docs crates/compiler/builtins/roc/main.roc
mv generated-docs/*.js www/build # move all .js files to build/
mv generated-docs/*.css www/build
mv generated-docs/*.svg www/build

mv generated-docs/ www/build/builtins # move all the rest to build/builtins/

# Manually add this tip to all the builtin docs.
find www/build/builtins -type f -name 'index.html' -exec sed -i 's!</nav>!<div class="builtins-tip"><b>Tip:</b> <a href="/different-names">Some names</a> differ from other languages.</div></nav>!' {} \;


# cleanup files that could have stayed behind if the script failed
rm -rf roc_nightly roc_releases.json

# we use `! [ -v GITHUB_TOKEN_READ_ONLY ];` to check if we're on a netlify server
if ! [ -v GITHUB_TOKEN_READ_ONLY ]; then
  echo 'Building tutorial.html from tutorial.md...'
  mkdir www/build/tutorial

  cargo build --release --bin roc

  roc=target/release/roc
else
  echo 'Fetching latest roc nightly...'
  
  # get roc release archive
  curl -fOL https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-linux_x86_64-latest.tar.gz
  # extract archive
  ls | grep "roc_nightly" | xargs tar -xzvf
  # delete archive
  ls | grep "roc_nightly.*tar.gz" | xargs rm
  # simplify dir name
  mv roc_nightly* roc_nightly

  roc='./roc_nightly/roc'

  echo 'Building tutorial.html from tutorial.md...'
  mkdir www/build/tutorial
fi

$roc version
$roc run www/generate_tutorial/src/tutorial.roc -- www/generate_tutorial/src/input/ www/build/tutorial/
mv www/build/tutorial/tutorial.html www/build/tutorial/index.html

# for new wip site
mkdir www/build/wip
$roc run www/wip_new_website/main.roc -- www/wip_new_website/content/ www/build/wip
cp -r www/wip_new_website/static/site.css www/build/wip

# cleanup
rm -rf roc_nightly roc_releases.json

echo 'Generating CLI example platform docs...'
# Change ROC_DOCS_ROOT_DIR=builtins so that links will be generated relative to
# "/packages/basic-cli/" rather than "/builtins/"
export ROC_DOCS_URL_ROOT=/packages/basic-cli

rm -rf ./downloaded-basic-cli

git clone --depth 1 https://github.com/roc-lang/basic-cli.git downloaded-basic-cli

cargo run --bin roc-docs downloaded-basic-cli/src/main.roc

rm -rf ./downloaded-basic-cli

BASIC_CLI_PACKAGE_DIR="www/build/packages/basic-cli"
mkdir -p $BASIC_CLI_PACKAGE_DIR
mv generated-docs/* $BASIC_CLI_PACKAGE_DIR # move all the folders to build/packages/basic-cli

# set up docs for basic-cli 0.3.2
BASIC_CLI_DIR_0_3_2=$BASIC_CLI_PACKAGE_DIR/0.3.2
mkdir -p $BASIC_CLI_DIR_0_3_2
curl -fL --output $BASIC_CLI_DIR_0_3_2/docs.tar.gz https://github.com/roc-lang/basic-cli/releases/download/0.3.2/docs.tar.gz
tar -xf $BASIC_CLI_DIR_0_3_2/docs.tar.gz -C $BASIC_CLI_DIR_0_3_2/
rm $BASIC_CLI_DIR_0_3_2/docs.tar.gz
mv $BASIC_CLI_DIR_0_3_2/generated-docs/* $BASIC_CLI_DIR_0_3_2
rm -rf $BASIC_CLI_DIR_0_3_2/generated-docs

popd
