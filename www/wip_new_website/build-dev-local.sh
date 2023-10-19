#!/usr/bin/env bash

# Use this script to for testing the WIP site locally without downloading assets every time. 

# NOTE run `bash www/build.sh` to cache local copy of fonts, and repl assets etc 

rm -rf dist/
mkdir -p dist/wip
cp -r ../build/wip/* dist/wip/
roc run main.roc -- content/ dist/wip/
cp -r ../build/fonts/ dist/fonts/
cp -r static/* dist/wip/

simple-http-server -p 8080 --nocache --index -- dist/ 
