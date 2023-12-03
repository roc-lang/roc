#!/usr/bin/env bash

# compile everything needed for benchmarks and output a self-contained dir from which benchmarks can be run.

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# to make use of avx, avx2, sse2, sse4.2... instructions
RUSTFLAGS="-C link-arg=-fuse-ld=lld -C target-cpu=native"
BENCH_SUFFIX=$1

cargo criterion -V
cd crates/cli && cargo criterion --no-run && cd ../..
mkdir -p bench-folder/crates/cli_testing_examples/benchmarks
mkdir -p bench-folder/crates/compiler/builtins/bitcode/src
mkdir -p bench-folder/target/release/deps
cp "crates/cli_testing_examples/benchmarks/"*".roc" bench-folder/crates/cli_testing_examples/benchmarks/
cp -r crates/cli_testing_examples/benchmarks/platform bench-folder/crates/cli_testing_examples/benchmarks/
cp crates/compiler/builtins/bitcode/src/str.zig bench-folder/crates/compiler/builtins/bitcode/src
cp target/release/roc bench-folder/target/release

# copy the most recent time bench to bench-folder
cp target/release/deps/`ls -t target/release/deps/ | grep time_bench | head -n 1` bench-folder/target/release/deps/time_bench
mv bench-folder bench-folder-$BENCH_SUFFIX