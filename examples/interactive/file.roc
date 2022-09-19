app "file-io"
    packages { pf: "cli-platform/main.roc" }
    imports [
        pf.Program.{ Program },
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
        pf.Env,
        pf.Dir,
    ]
    provides [main] to pf

main : Program
main = Program.noArgs mainTask

mainTask : Task U8 [] [Write [File, Stdout, Stderr], Read [File], Env]
mainTask =
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
        msg = when result is
            Err (FileWriteErr _ PermissionDenied) -> Err "PermissionDenied"
            Err (FileWriteErr _ Unsupported) -> Err "Unsupported"
            Err (FileWriteErr _ (Unrecognized _ other)) -> Err other
            Err (FileReadErr _ _) -> Err "Error reading file"
            Err _ -> Err "Uh oh, there was an error!"
            Ok _ -> Ok "Successfully wrote a string to out.txt"

        when msg is
            Ok ok -> Task.await (Stdout.line ok) \{} -> Task.succeed 0
            Err err -> Task.await (Stderr.line err) \{} -> Task.succeed 1
