#! /usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

cargo run -- build
mkdir -p SwiftUIDemo.app/Contents/MacOS/
mv swiftui SwiftUIDemo.app/Contents/MacOS/SwiftUIDemo
open SwiftUIDemo.app