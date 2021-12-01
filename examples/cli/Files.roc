#!/usr/bin/env roc

app "echo"
    packages { base: "platform" }
    imports [ base.Task.{ Task, await }, base.Stdout, base.Stdin, base.File ]
    provides [ main ] to base

main : Task {} []
main =
    task =
        {} <- await (Stdout.line "What file should I read?")

        filename <- await Stdin.line

        File.readUtf8 filename

    Task.attempt task \result ->
        when result is
            Ok contents -> Stdout.line "Here are its contents:\n\n\(contents)"
            Err _ -> Stdout.line "Error reading file!"
