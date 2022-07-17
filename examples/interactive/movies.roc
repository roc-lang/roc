app "echo"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Stderr, pf.Task.{ Task }, pf.Path, pf.File,
        Encode.{ toEncoder }, Json
    ]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout, Disk]]
main =
    lst = [{a: "foo"}, {a: "bar"}, {a: "baz"}]
    encoded = Encode.toBytes lst Json.format
    task = File.writeBytes (Path.fromStr "test.txt") encoded

    Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "Wrote the file!"
            Err (FileWriteErr _) -> Stderr.line "Error writing to file"
            # Err (FileWriteErr (NotFound _)) -> Task.succeed {}
            # Err (FileWriteErr (FileWasDir _)) -> Task.succeed {}
