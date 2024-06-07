app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }

import pf.Http
import pf.Task exposing [Task]
import pf.Stdout

main =
    request = {
        method: Get,
        headers: [],
        url: "http://www.example.com",
        mimeType: "",
        body: [],
        timeout: TimeoutMilliseconds 5000,
    }

    resp = Http.send! request

    output =
        when resp |> Http.handleStringResponse is
            Err err -> crash (Http.errorToString err)
            Ok body -> body

    Stdout.line output
