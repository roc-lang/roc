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

rm -rf content/examples/
# download the latest code for the examples
echo 'Downloading latest examples...'
curl -fL -o examples-main.zip https://github.com/roc-lang/examples/archive/refs/heads/main.zip
rm -rf examples-main/
unzip -o -q examples-main.zip
cp -R examples-main/examples/ content/examples/

# replace links in content/examples/index.md to work on the WIP site
perl -pi -e 's|\]\(/|\]\(/examples/|g' content/examples/index.md

# clean up examples artifacts
rm -rf examples-main examples-main.zip

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
mkdir repl
tar -xzf $REPL_TARFILE -C repl
rm $REPL_TARFILE
ls -lh repl

popd

pushd ..
cargo --version

# We set ROC_DOCS_ROOT_DIR=builtins so that links will be generated relative to
# "/builtins/" rather than "/" - which is what we want based on how the server
# is set up to serve them.
export ROC_DOCS_URL_ROOT=/builtins

cargo run --release --bin roc-docs crates/compiler/builtins/roc/main.roc

mv generated-docs/ www/build/builtins # move everything to build/builtins/

# Manually add this tip to all the builtin docs.
find www/build/builtins -type f -name 'index.html' -exec sed -i 's!</nav>!<div class="builtins-tip"><b>Tip:</b> <a href="/different-names">Some names</a> differ from other languages.</div></nav>!' {} \;


# cleanup files that could have stayed behind if the script failed
rm -rf roc_nightly roc_releases.json

# we use `! [ -v GITHUB_TOKEN_READ_ONLY ];` to check if we're on a netlify server
if ! [ -v GITHUB_TOKEN_READ_ONLY ]; then
  cargo build --release --bin roc

  roc=target/release/roc
else
  echo 'Fetching latest roc nightly...'

  # get roc release archive
  LATEST_URL="https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-linux_x86_64-latest.tar.gz"
  curl -fOL "$LATEST_URL"
  
  # extract archive
  ls | grep "roc_nightly" | xargs tar -xzvf
  # delete archive
  ls | grep "roc_nightly.*tar.gz" | xargs rm
  # simplify dir name
  mv roc_nightly* roc_nightly

  roc='./roc_nightly/roc'
fi

$roc version

echo 'Generating site markdown content'
$roc build --linker legacy www/main.roc
./www/main www/content/ www/build/

echo "Adding github link to examples' html..."
source www/scripts/add-github-link-to-examples.sh
add_github_link_to_examples www/build/examples

# cleanup
rm -rf roc_nightly roc_releases.json
