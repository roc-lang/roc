#!/usr/bin/env roc

app "echo"
    packages { base: "platform" }
    imports [ base.Task.{ Task, await }, base.Stdout ]
    provides [ main ] to base

main : Task {} *
main =
    {} <- await (Stdout.line "One")

    {} <- await (Stdout.line "Two")

    Stdout.line "Three"
