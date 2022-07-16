app "http-get"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Http, pf.Task]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout], Network [Http]]
main =
    # request : Request
    # request = { defaultRequest & url: "https://httpbin.org/get" }

    # result <- Http.send request |> Task.await
    # output =
    #     when result is
    #         Ok payload -> payload
    #         Err httpError -> Http.errorToString httpError
    output = "Hello"
    Stdout.line output
