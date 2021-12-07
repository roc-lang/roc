#!/usr/bin/env roc

app "echo"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Stdin, pf.File ]
    provides [ main ] to pf

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
