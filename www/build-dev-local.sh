#!/usr/bin/env bash

# Use this script to for testing the WIP site locally without downloading assets every time.

# NOTE run `bash www/build.sh` to cache local copy of fonts, and repl assets etc

## Get the directory of the currently executing script
DIR="$(dirname "$0")"

# Change to that directory
cd "$DIR" || exit

rm -rf dist/
cp -r build dist/
cp -r public/* dist/
roc run main.roc -- content/ dist/

npx http-server dist/ -p 8080 -c-1 --cors
