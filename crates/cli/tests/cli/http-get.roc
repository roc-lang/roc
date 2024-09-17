app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

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
