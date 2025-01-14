#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

# Use this script to for testing the WIP site locally without downloading assets every time.

# NOTE run `bash www/build.sh` to cache local copy of fonts, and repl assets etc

## Get the directory of the currently executing script
DIR="$(dirname "$0")"

# Change to that directory
cd "$DIR" || exit

rm -rf dist/
cp -r build dist/
cp -r public/* dist/
roc main.roc --linker=legacy -- content/ dist/

simple-http-server -p 8080 --nocache --cors --index -- dist/
