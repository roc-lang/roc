app "http-example"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Stderr, pf.Http, pf.File ]
    provides [ main ] to pf


main : Task {} []
main =
    task =
        username <- await (File.read "username.txt")
        userData <- await (Http.get "http://localhost:8000/\(username)")
        File.write "userData.txt" userData

    Task.attempt task \result ->
        when result is
            Ok _ -> Stdout.line "Success!"
            Err (FileReadErr (NotFound path)) -> Stderr.line "Not found: \(path)"
            Err (FileReadErr _) -> Stderr.line "File read error!"
            Err (FileWriteErr _) -> Stderr.line "File write error!"
            Err (HttpGetErr (Bad url)) -> Stderr.line "\(url) did not send UTF-8"
            Err (HttpGetErr (Status 404 url)) -> Stderr.line "Not found: \(url)!"
            Err (HttpGetErr (Status 500 url)) -> Stderr.line "Internal server error: \(url)"
            Err (HttpGetErr (Status status url)) ->
                statusStr = Str.fromInt status

                Stderr.line "GET \(url) returned error \(statusStr)!"
