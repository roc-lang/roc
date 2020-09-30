#!/bin/bash

set -euo pipefail

# THIS SCRIPT MUST BE RUN FROM "compiler/builtins/bitcode", IT USES PATHS RELATIVE TO THAT TO DO IT'S WORK.
#
# This script generates the bitcode for roc builtins and it's dependencies:
#
# 1. Deletes any existing bitcode output files. Sometimes if these are there, rustc
#    doesn't generate the .bc file - or we can end up with more than one .bc.
# 2. Regenerate the .bc files for each dependency, then the roc builtins
# 3. Deletes the current bitcode file used in the roc `gen` crate
# 4. Link all of the compiled bitcode files together & save it to the  roc `gen` crate
#
# We do all of this in a separate function so we can declare local variables.


# Uncomment this line when modifying this script. I logs every command and is really helpful!
# set -x


function gen_bitcode() {
  # Setup paths based on relative dir
  local roc_root_dir=$(cd ../../../ && pwd)
  local roc_bitcode_dir="$roc_root_dir/compiler/builtins/bitcode"
  local roc_target_dir="$roc_root_dir/target/release/deps"

  # Delete previously generated bitcode
  if test -d "$roc_target_dir"; then
    rm -rf "$roc_target_dir" > /dev/null
  fi

  echo "Compiling roc builtins..."
  cargo rustc --release --lib -- --emit=llvm-bc -Clinker-plugin-lto

  local libm_rlib=$(ls -d "$roc_target_dir"/{*,.*} | grep "libm.*rlib")
  local libm_rlib_len=$(echo "$libm_rlib" | wc -l)
  if [[ "$libm_rlib_len" != 1 ]]; then
    echo "More than one libm .rlib file was emitted somehow."
    exit 1;
  fi

  unarchive_dir="$(mktemp -d)"
  trap "rm -rf $unarchive_dir" INT TERM EXIT
  pushd "$unarchive_dir" > /dev/null
  ar x "$libm_rlib" > /dev/null
  local libm_bc=$(ls | grep "libm.*o")
  local libm_bc_len=$(echo "$libm_bc" | wc -l)
  if [[ "$libm_bc_len" != 1 ]]; then
    echo "More than one libm .o file was emitted somehow."
    exit 1;
  fi
  local libm_bc="$PWD/$libm_bc"
  popd > /dev/null

  local builtin_bc=$(ls -d "$roc_target_dir"/{*,.*} | grep "roc_builtins_bitcode.*bc")
  local builtin_bc_len=$(echo "$builtin_bc" | wc -l)
  if [[ "$builtin_bc_len" != 1 ]]; then
    echo "More than one builtins .bc file was emitted somehow."
    exit 1;
  fi

  echo "Done! Compiled roc builtins bitcode."
  printf "\n"

  # Remove old builtins bitcode file
  local gen_builtins_bc="$roc_root_dir/compiler/gen/src/llvm/builtins.bc"
  if test -f "$gen_builtins_bc"; then
    rm "$gen_builtins_bc" > /dev/null
  fi

  echo "Linking bitcode..."
  # Link the files together
  # $builtis_bc must be the last bitcode arg here
  llvm-link "$libm_bc" "$builtin_bc" -o "$gen_builtins_bc"
  echo "Done! The linked bitcode has been saved to \"$gen_builtins_bc\"."
}

gen_bitcode
