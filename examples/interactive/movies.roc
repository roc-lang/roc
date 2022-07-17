app "movies"
    packages { pf: "cli-platform/main.roc" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.Path,
        pf.File,
        pf.Url,
        pf.Http,
        pf.Env,
        Encode,
        Json,
    ]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout, Disk], Net, Env]
main =
    lst = [{ title: "foo", starring: "blah" }, { title: "bar", starring: "foo" }]
    task =
        apiKey <- Env.varUtf8 "API_KEY" |> Task.withDefault "" |> Task.await
        str <- Http.getUtf8 (Url.fromStr "http://localhost:4000/movies?key=\(apiKey)") |> Task.await

        _ <- Stdout.line str |> Task.await

        Path.fromStr "test.txt"
            |> write lst Json.format

    Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "Wrote the file!"
            Err (HttpGetErr _) -> Stderr.line "Error reading from URL"
            Err (FileWriteErr _) -> Stderr.line "Error writing to file"
# Err (FileWriteErr (NotFound _)) -> Task.succeed {}
# Err (FileWriteErr (FileWasDir _)) -> Task.succeed {}


# TODO annotating this with write : {} throws a compiler panic
write = \path, val, fmt ->
    File.writeBytes path (Encode.toBytes val fmt)
