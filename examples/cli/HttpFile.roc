app "http-example"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Http, pf.File ]
    provides [ main ] to pf


main : Task {} []
main =
    (
        username <- await (File.readUtf8 "userData.txt")
        userData <- await (Http.getUtf8 "http://localhost:8000/\(username)")
        File.writeUtf8 "userData.txt" userData
    )
        |> Task.attempt \result ->
            when result is
                Ok _ -> Stdout.line "Success!"
                Err (FileReadErr _) -> Stdout.line "File read error!"
                Err (FileWriteErr _) -> Stdout.line "File write error!"
                Err (HttpGetErr _) -> Stdout.line "HTTP GET error!"
