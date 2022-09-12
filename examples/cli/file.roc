app "echo"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Stderr, pf.Task, pf.File, pf.Path]
    provides [main] to pf

main : Task.Task {} [] [Write [File, Stdout, Stderr]]
main =
    task =
        _ <- Stdout.line "Writing a string to out.txt" |> Task.await
        File.writeUtf8 (Path.fromStr "out.txt") "a string!\n"

    Task.attempt task \result ->
        when result is
            Err (FileWriteErr _ PermissionDenied) -> Stderr.line "Err: PermissionDenied"
            Err (FileWriteErr _ Unsupported) -> Stderr.line "Err: Unsupported"
            Err (FileWriteErr _ (Unrecognized _ other)) -> Stderr.line "Err: \(other)"
            _ -> Stdout.line "Successfully wrote a string to out.txt"