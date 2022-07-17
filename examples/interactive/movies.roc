app "echo"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Stderr, pf.Task.{ Task }, pf.Path, pf.File]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout, Disk]]
main =
    task = File.writeUtf8 (Path.fromStr "test.txt") "this is a test!"

    Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "Wrote the file!"
            Err (FileWriteErr _) -> Stderr.line "Error writing to file"
            # Err (FileWriteErr (NotFound _)) -> Task.succeed {}
            # Err (FileWriteErr (FileWasDir _)) -> Task.succeed {}
