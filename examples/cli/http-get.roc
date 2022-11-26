app "http-get"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.0/_V6HO2Dwez0xsSstgK8qC6wBLXSfNlVFyUTMg0cYiQQ.tar.br" }
    imports [pf.Http, pf.Task.{ Task }, pf.Stdin, pf.Stdout]
    provides [main] to pf

main : Task {} []
main =
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
