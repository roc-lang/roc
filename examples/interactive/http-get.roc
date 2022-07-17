app "http-get"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Http, pf.Task, pf.Stdout]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout], Network [Http]]
main =
    request = {
        method: "GET",
        headers: [],
        url: "https://httpbin.org/get",
        body: Http.emptyBody,
        timeout: NoTimeout,
        tracker: NoTracker,
        allowCookiesFromOtherDomains: False,
    }

    output <- Http.send request
            |> Task.onFail (\err -> err |> Http.errorToString |> Task.succeed)
            |> Task.await

    Stdout.line output
