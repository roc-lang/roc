app [main] { pf: platform "https://github.com/roc-lang/basic-webserver/releases/download/0.1/dCL3KsovvV-8A5D_W_0X_abynkcRcoAngsgF0xtvQsk.tar.br" }

import pf.Stdout
import pf.Task exposing [Task]
import pf.Http exposing [Request, Response]
import pf.Utc

main : Request -> Task Response []
main = \req ->

    # Log request date, method and url
    date <- Utc.now |> Task.map Utc.toIso8601Str |> Task.await
    {} <- Stdout.line "$(date) $(Http.methodToStr req.method) $(req.url)" |> Task.await

    # Respond with request body
    when req.body is
        EmptyBody -> Task.ok { status: 200, headers: [], body: [] }
        Body internal -> Task.ok { status: 200, headers: [], body: internal.body }

