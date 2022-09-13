app "file-io"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Stderr, pf.Task, pf.File, pf.Path, pf.Env, pf.Dir]
    provides [main] to pf

main : Task.Task {} [] [Write [File, Stdout, Stderr], Read [File], Env]
main =
    path = Path.fromStr "out.txt"
    task =
        cwd <- Env.cwd |> Task.await
        cwdStr = Path.display cwd

        _ <- Stdout.line "cwd: \(cwdStr)" |> Task.await
        dirEntries <- Dir.list cwd |> Task.await
        contentsStr = Str.joinWith (List.map dirEntries Path.display) "\n    "

        _ <- Stdout.line "Directory contents:\n    \(contentsStr)\n" |> Task.await
        _ <- Stdout.line "Writing a string to out.txt" |> Task.await
        _ <- File.writeUtf8 path "a string!" |> Task.await
        contents <- File.readUtf8 path |> Task.await
        Stdout.line "I read the file back. Its contents: \"\(contents)\""

    Task.attempt task \result ->
        when result is
            Err (FileWriteErr _ PermissionDenied) -> Stderr.line "Err: PermissionDenied"
            Err (FileWriteErr _ Unsupported) -> Stderr.line "Err: Unsupported"
            Err (FileWriteErr _ (Unrecognized _ other)) -> Stderr.line "Err: \(other)"
            Err (FileReadErr _ _) -> Stderr.line "Error reading file"
            Err _ -> Stderr.line "Uh oh, there was an error!"
            Ok _ -> Stdout.line "Successfully wrote a string to out.txt"
