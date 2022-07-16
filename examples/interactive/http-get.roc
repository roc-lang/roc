app "http-get"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Http, pf.HttpTypes, pf.Task, pf.Stdout]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout], Network [Http]]
main =
     Http.send { Http.defaultRequest & url: "https://httpbin.org/get" }
        |> Task.onFail (\err -> err |> Http.errorToString |> Task.succeed)
        |> Task.await
        |> Stdout.line
