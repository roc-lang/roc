#!/usr/bin/env roc

app "reverse"
    packages { base: "platform" }
    imports []
    provides [ reverse ] to base

reverse : List Str -> List Str
reverse = \list -> List.reverse list
