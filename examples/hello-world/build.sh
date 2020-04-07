#!/bin/bash
set -Eeuo pipefail

cargo run hello.roc
ar rcs libhello_from_roc.a hello.o
rustc -L . host.rs -o hello

./hello
