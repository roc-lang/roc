app "echo"
    packages { pf: "cli-platform/main.roc" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.Path,
        pf.File,
        Encode,
        Json,
    ]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout, Disk]]
main =
    lst = [{ a: "foo" }, { a: "bar" }, { a: "baz" }]
    encoded = Encode.toBytes lst Json.format
    # task = File.writeBytes (Path.fromStr "test.txt") encoded
    task = write (Path.fromStr "test.txt") encoded

    Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "Wrote the file!"
            Err (FileWriteErr _) -> Stderr.line "Error writing to file"
# Err (FileWriteErr (NotFound _)) -> Task.succeed {}
# Err (FileWriteErr (FileWasDir _)) -> Task.succeed {}

# write : Path, val, fmt -> Task {} (FileWriteErr *) [Write [Disk]*]*
#     | val has Encode.Encoding, fmt has Encode.EncoderFormatting
write = \path, val, fmt ->
    File.writeBytes path (Encode.toBytes val fmt)
