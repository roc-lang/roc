#!/usr/bin/env bash

# Use this script to for testing the WIP site locally without downloading assets every time.

# NOTE run `bash www/build.sh` to cache local copy of fonts, and repl assets etc

## Get the directory of the currently executing script
DIR="$(dirname "$0")"

# Change to that directory
cd "$DIR" || exit

rm -rf dist/
cp -r ../build dist/
mkdir -p dist/wip
roc run main.roc -- content/ dist/wip/
cp -r static/* dist/wip/

npx http-server dist/ -p 8080 -c-1 --cors
