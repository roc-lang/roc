#!/bin/bash

SCRIPT_DIR=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)
${SCRIPT_DIR}/../../target/debug/roc_ls "$@" 2> /tmp/rocls.err
