app "echo"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Stderr, pf.Task.{ Task }, pf.Path, pf.File]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout, Disk]]
main =
    task =
        _ <- Stdout.line "Writing the file..." |> Task.await
        File.writeUtf8 (Path.fromStr "test.txt") "this is a test!"

    result <- Task.attempt task

    when result is
        Ok {} -> Stdout.line "Wrote the file!"
        Err _ -> Stderr.line "Error!"
        # Err (FileWriteErr (NotFound path)) ->
        #     pathStr = Path.display path
        #     Stderr.line "Not found: \(pathStr)"
        # Err (FileWriteErr (FileWasDir path)) ->
        #     pathStr = Path.display path
        #     Stderr.line "Path was a directory, not a file: \(pathStr)"


