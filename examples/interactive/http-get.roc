app "http-get"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Http, pf.Task, pf.Stdin, pf.Stdout]
    provides [main] to pf

main : List Str -> Task.Task {} [] [Read [Stdin], Write [Stdout], Network [Http]]
main = \_args ->
    _ <- Task.await (Stdout.line "Please enter a URL to fetch")

    url <- Task.await Stdin.line

    request = {
        method: Get,
        headers: [],
        url,
        body: Http.emptyBody,
        timeout: NoTimeout,
    }

    output <- Http.send request
        |> Task.onFail (\err -> err |> Http.errorToString |> Task.succeed)
        |> Task.await

    Stdout.line output
