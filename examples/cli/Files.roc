#!/usr/bin/env roc

app "echo"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Stdin, pf.File ]
    provides [ main ] to pf

main : Task {} []
main =
    Task.attempt mainHelp \result ->
        when result is
            Err e ->
                Stdout.line "something went wrong"

            Ok v ->
                Task.succeed v

mainHelp : Task {} File.ReadErr
mainHelp =
    task =
        {} <- await (Stdout.line "What file should I read?")
        filename <- await Stdin.line
        File.readUtf8Infallible filename

    str <- await task

    Stdout.line "it was: \(str)"

    # Task.attempt task \result ->
    #     when result is
    #         Ok contents -> Stdout.line "Here are its contents:\n\n\(contents)"
    #         Err _ -> Stdout.line "Error reading file!"
