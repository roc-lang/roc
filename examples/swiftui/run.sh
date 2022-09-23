#! /usr/bin/env bash

cargo run -- build
mkdir -p SwiftUIDemo.app/Contents/MacOS/
mv swiftui SwiftUIDemo.app/Contents/MacOS/SwiftUIDemo
open SwiftUIDemo.app