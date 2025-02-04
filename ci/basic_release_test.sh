#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# Check if argument is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <tar-file>"
    exit 1
fi

RELEASE_TAR_FILE="$1"

# Check if tar file exists
if [ ! -f "$RELEASE_TAR_FILE" ]; then
    echo "Error: Tar file '$RELEASE_TAR_FILE' not found"
    exit 1
fi

mkdir -p roc_release

# decompress the tar
tar -xzvf "$RELEASE_TAR_FILE" -C roc_release

# delete tar
rm -rf "$RELEASE_TAR_FILE"

cd roc_release/*/

# test rust platform (first prebuild the host)
# temp disabled
# examples/platform-switching/rust-platform/build.sh
# ./roc examples/platform-switching/rocLovesRust.roc

# test zig platform
./roc --build-host --suppress-build-host-warning examples/platform-switching/rocLovesZig.roc

# test C platform
./roc --build-host --suppress-build-host-warning examples/platform-switching/rocLovesC.roc

# test repl
cd ../../ci/repl_basic_test
cargo build --release
cp target/release/repl_basic_test ../../roc_release/*/
cd ../../roc_release/*/
./repl_basic_test

cd ../..
#cleanup
rm -rf roc_release