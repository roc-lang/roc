#!/bin/bash

# Runs on every Netlify build, to set up the Netlify server.

set -euxo pipefail

rustup update
rustup default stable

# TODO remove this once we actually build the web repl!
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
REPL_WASM_DATA=${SCRIPT_DIR}/../repl_wasm/data/
mkdir -p ${REPL_WASM_DATA}
touch ${REPL_WASM_DATA}/pre_linked_binary.o

bash build.sh
