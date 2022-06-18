#!/usr/bin/env bash
cd ./target/release
tar -czvf $1 ./roc ../../LICENSE ../../LEGAL_DETAILS ../../examples/hello-world ../../compiler/builtins/bitcode/src/ ../../roc_std