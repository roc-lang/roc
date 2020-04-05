#!/bin/bash
set -Eeuo pipefail

cargo run qs.roc
gcc -shared qs.o -o libroc_qs_main.so

# Move it to a different place depending on Linux vs macOS
unameVal="$(uname -s)"

case "${unameVal}" in
    Linux*)     sudo mv libroc_qs_main.so /usr/lib/;;
    Darwin*)    sudo mv libroc_qs_main.so /usr/local/lib/;;
    *)          echo "build.sh does not support this operating system!" exit 1;
esac

rustc host.rs -o qs

./qs
