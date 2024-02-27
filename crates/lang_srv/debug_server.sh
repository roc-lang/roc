#!/usr/bin/bash

SCRIPT_DIR=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)
RUST_LOG=debug
${SCRIPT_DIR}/../../target/debug/roc_language_server "$@" 2> /tmp/roc_language_server.err
