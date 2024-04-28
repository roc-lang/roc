app "http-get"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }
    imports [pf.Http, pf.Task, pf.Stdin, pf.Stdout]
    provides [main] to pf

main =
    Stdout.line! "Enter a URL to fetch. It must contain a scheme like \"http://\" or \"https://\"."

    url = Stdin.line!

    request = {
        method: Get,
        headers: [],
        url,
        mimeType: "",
        body: [],
        timeout: NoTimeout,
    }

    output =
        Http.send request
        |> Task.await \resp -> resp |> Http.handleStringResponse |> Task.fromResult
        |> Task.onErr! \err -> crash (Http.errorToString err)

    Stdout.line! output
