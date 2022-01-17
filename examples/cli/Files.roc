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
        File.readUtf8Infallible filename

    Task.attempt task \result ->
        when result is
            Err err ->
                msg =
                    when err is
                        FileBusy px -> "FileBusy  \(px)"
                        FileWasDir px -> "FileWasDir  \(px)"
                        IllegalByteSequence  px -> "IllegalByteSequence   \(px)"
                        InvalidSeek  px -> "InvalidSeek   \(px)"

                Stdout.line "Error reading file: \(msg)"

            Ok contents ->
                {} <- await (Stdout.line "Here are its contents:\n\n\(contents)")
                Task.succeed {}
