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

# test roc hello world
./roc examples/helloWorld.roc 

./roc examples/platform-switching/rocLovesRust.roc

./roc examples/platform-switching/rocLovesZig.roc

./roc examples/platform-switching/rocLovesC.roc

cd ..
