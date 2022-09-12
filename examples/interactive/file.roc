app "file_io"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Stderr, pf.Task, pf.File, pf.Path]
    provides [main] to pf

main : List Str -> Task.Task {} [] [Write [File, Stdout, Stderr]]
main = \args ->
    filename = List.get args 1 |> Result.withDefault "out.txt"
    content = List.get args 2 |> Result.withDefault "a string!\n"

    task =
        _ <- Stdout.line "Writing to `\(filename)`..." |> Task.await
        _ <- File.writeUtf8 (Path.fromStr filename) content |> Task.await
        Stdout.line "Done!"

    Task.attempt task \result ->
        when result is
            Err (FileWriteErr _ PermissionDenied) -> Stderr.line "Err: PermissionDenied"
            Err (FileWriteErr _ Unsupported) -> Stderr.line "Err: Unsupported"
            Err (FileWriteErr _ (Unrecognized _ other)) -> Stderr.line "Err: \(other)"
            _ -> Stdout.line "Successfully wrote a string to out.txt"