#!/bin/bash

# Run this script to regenerate the glue_for_glue crate.
# It only uses the generated roc_app/ directory, which in turn
# gets roc_std from this source tree. (This script deletes the generated roc_std/ dir.)

set -euf -o pipefail

# This script's directory
DIR="$(dirname "$(realpath "$0")")"

# Generate the new glue, and if that succeeded, replace the current package with it
roc glue "$DIR/src/RustGlue.roc" ./new_glue "$DIR/platform/main.roc"
rm -rf "$DIR/../glue_for_glue"
mv ./new_glue/roc_app "$DIR/../glue_for_glue"
rm -rf ./new_glue

# Update the generated crate name to roc_glue_for_glue
sed -i '/^name = /s/"[^"]*"/"roc_glue_for_glue"/' "$DIR/../glue_for_glue/Cargo.toml"
