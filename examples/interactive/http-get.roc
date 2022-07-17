app "http-get"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Http, pf.Task, pf.Stdout]
    provides [main] to pf

main : Task.Task {} [] [Write [Stdout], Network [Http]]
main =
    request = {
        method: "GET",
        headers: [Header "Favourite-Colour" "Mauve"],
        url: "https://httpbin.org/get",
        body: Http.stringBody (MimeType "text/plain") "Hello, I am the body text",
        timeout: Timeout 1.0,
        tracker: Tracker "some-progress-tracking-identifier",
        allowCookiesFromOtherDomains: True,
    }

    output <- Http.send request
            |> Task.onFail (\err -> err |> Http.errorToString |> Task.succeed)
            |> Task.await

    Stdout.line output
