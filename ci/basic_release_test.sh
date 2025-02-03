#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# Check if argument is provided
if [ $# -lt 1 ]; then
    echo "Usage: $0 <os> [old]"
    exit 1
fi

OS="$1"
IS_OLD="${2:-}"

function download_release() {
    local os="$1"
    local is_old="$2"
    local old_prefix=""
    
    if [ "$is_old" = "old" ]; then
        old_prefix="old_"
    fi

    case "$os" in
        "macos-13")
            curl -fL "https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-macos_x86_64-latest.tar.gz" -o roc_release.tar.gz
            ;;
        "macos-14")
            curl -fL "https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-macos_apple_silicon-latest.tar.gz" -o roc_release.tar.gz
            ;;
        ubuntu-*-arm)
            curl -fL "https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-${old_prefix}linux_arm64-latest.tar.gz" -o roc_release.tar.gz
            ;;
        ubuntu-*)
            curl -fL "https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-${old_prefix}linux_x86_64-latest.tar.gz" -o roc_release.tar.gz
            ;;
        *)
            echo "Unsupported OS: $os"
            exit 1
            ;;
    esac
}

# Download the appropriate release
download_release "$OS" "$IS_OLD"

mkdir -p roc_release

# decompress the tar
tar -xzvf roc_release.tar.gz -C roc_release

# delete tar
rm -rf roc_release.tar.gz

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