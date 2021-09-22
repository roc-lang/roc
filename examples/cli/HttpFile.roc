app "http-example"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Stderr, pf.Http, pf.File ]
    provides [ main ] to pf


main : Task {} []
main =
    task =
        username <- await (File.readUtf8 "username.txt")
        userData <- await (Http.getUtf8 "http://localhost:8000/\(username)")
        File.writeUtf8 "userData.txt" userData

    Task.attempt task \result ->
        when result is
            Ok _ -> Stdout.line "Success!"
            Err (FileReadUtf8Err (NotFound path)) -> Stderr.line "Not found: \(path)"
            Err (FileReadUtf8Err _) -> Stderr.line "File read error!"
            Err (FileWriteErr _) -> Stderr.line "File write error!"
            Err (HttpGetUtf8Err (BadUtf8 url)) -> Stderr.line "\(url) did not send UTF-8"
            Err (HttpGetUtf8Err (Status 404 url)) -> Stderr.line "Not found: \(url)!"
            Err (HttpGetUtf8Err (Status 500 url)) -> Stderr.line "Internal server error: \(url)"
            Err (HttpGetUtf8Err (Status status url)) ->
                statusStr = Str.fromInt status

                Stderr.line "GET \(url) returned error \(statusStr)!"
