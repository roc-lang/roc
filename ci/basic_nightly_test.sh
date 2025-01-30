#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# if to prevent unset vars errror
if [ -n "$(ls | grep -v "roc_nightly.*tar\.gz"  | grep -v "^ci$")" ]; then

  # Remove everything in this dir except the tar and ci folder.
  # We want to test like a user who would have downloaded the release, so we clean up all files from the repo checkout.
  to_delete=$(ls | grep -v "roc_nightly.*tar\.gz"  | grep -v "^ci$")

  for file_or_dir in $to_delete
  do
    echo "Removing: $file_or_dir"
    rm -rf "$file_or_dir"
  done
fi

# decompress the tar
ls | grep "roc_nightly.*tar\.gz" | xargs tar -xzvf

# delete tar
ls | grep "roc_nightly.*tar\.gz" | xargs rm -rf

# rename nightly folder
mv roc_nightly* roc_nightly

cd roc_nightly

# test rust platform (first prebuild the host)
# temp disabled
# examples/platform-switching/rust-platform/build.sh
# ./roc examples/platform-switching/roc_loves_Rust.roc

# test zig platform
./roc --build-host --suppress-build-host-warning examples/platform-switching/roc_loves_zig.roc

# test C platform
./roc --build-host --suppress-build-host-warning examples/platform-switching/roc_loves_c.roc

# test repl
cd ../ci/repl_basic_test
cargo build --release
cp target/release/repl_basic_test ../../roc_nightly
cd ../../roc_nightly
./repl_basic_test


cd ..
