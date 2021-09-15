app "http-example"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Http, pf.File ]
    provides [ main ] to pf


main : Task {} *
main =
    username <- await (File.readUtf8 "userData.txt")
    userData <- await (Http.getUtf8 "http://localhost:8000/\(username)")
    result <- Task.attempt (File.writeUtf8 "userData.txt" userData)

    when result is
        Ok _ -> Stdout.line "Success!"
        Err _ -> Stdout.line "Error!"
