#!/bin/bash
set -Eeuo pipefail

cargo run qs.roc
ar rcs libroc_qs_main.a qs.o
rustc -L . host.rs -o qs

./qs
