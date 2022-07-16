app "http-get"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Http, pf.HttpTypes, pf.Task, pf.Stdout]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout], Network [Http]]
main =
    request : HttpTypes.Request
    request = { Http.defaultRequest & url: "https://httpbin.org/get" }

    output <- Http.send request
            |> Task.onFail (\err -> err |> Http.errorToString |> Task.succeed)
            |> Task.await

    Stdout.line output
