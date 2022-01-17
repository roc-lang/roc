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
            Err (ReadUtf8 err) ->
                msg = fmtErr err
                Stdout.line "Error reading file: \(msg)"

            Ok contents ->
                # TODO remove this `await` once https://github.com/rtfeldman/roc/pull/2226 has landed
                {} <- await (Stdout.line "Here are its contents:\n\n\(contents)")
                Task.succeed {}

fmtErr : File.ReadUtf8Err -> Str
fmtErr = \err ->
    when err is
        FileBusy path -> "\(path) was busy"
        FileWasDir path -> "\(path) was a directory, not a file"
        IllegalByteSequence  path -> "\(path) contained an illegal byte sequence"
        InvalidSeek path -> "Invalid seek when reading from \(path)"
        BadUtf8 path _ _ -> "\(path) was not encoded as valid UTF-8"
