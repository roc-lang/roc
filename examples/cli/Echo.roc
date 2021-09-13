#!/usr/bin/env roc

app "echo"
    packages { base: "platform" }
    imports [ base.Task.{ Task, await }, base.Stdout, base.Effect ]
    provides [ main ] to base

main : Task {} *
main =
    Task.await (Stdout.line "One") \{} ->
        Task.await (Stdout.line "Two") \{} ->
            Stdout.line "Three"
