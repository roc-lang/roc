app "movies"
    packages { pf: "cli-platform/main.roc" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.Path,
        pf.File,
        Encode.{ toEncoder },
        Json,
    ]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout, Disk]]
main =
    lst = [{ title: "foo", starring: "blah" }, { title: "bar", starring: "foo" }]
    task =
        Path.fromStr "test.txt"
            |> write lst Json.format

    Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "Wrote the file!"
            Err (FileWriteErr _) -> Stderr.line "Error writing to file"
# Err (FileWriteErr (NotFound _)) -> Task.succeed {}
# Err (FileWriteErr (FileWasDir _)) -> Task.succeed {}


# TODO annotating this with write : {} throws a compiler panic
write = \path, val, fmt ->
    File.writeBytes path (Encode.toBytes val fmt)
