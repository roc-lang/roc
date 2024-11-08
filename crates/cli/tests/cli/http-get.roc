app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br" }

import pf.Http
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
