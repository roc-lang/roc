app "http-example"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Stderr, pf.Http, pf.File ]
    provides [ main ] to pf


main : Task {} []
main =
    result <- Task.attempt
        (
            username <- await (File.readUtf8 "userData.txt")
            userData <- await (Http.getUtf8 "http://localhost:8000/\(username)")
            File.writeUtf8 "userData.txt" userData
        )

    when result is
        Ok _ -> Stdout.line "Success!"
        Err (FileReadUtf8Err (FileNotFound path)) -> Stderr.line "Not found: \(path)"
        Err (FileReadUtf8Err _) -> Stderr.line "File read error!"
        Err (FileWriteErr _) -> Stderr.line "File write error!"
        Err (HttpGetErr _) -> Stderr.line "HTTP GET error!"
