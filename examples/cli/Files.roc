#!/usr/bin/env roc

app "echo"
    packages { base: "platform" }
    imports [ base.Task.{ Task, await }, base.Stdout, base.Stdin ]
    provides [ main ] to base

main : Task {} *
main =
    {} <- await (Stdout.line "What file should I read?")

    filename <- await Stdin.line

    result <- await File.

    when result is
        Ok contents -> Stdout.line "Here are its contents:\n\n\(contents)"
        Err _ -> Stdout.line "Error reading file!"
