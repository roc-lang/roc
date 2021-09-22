interface Http
    exposes [ Url, HttpErr, HttpGetUtf8Err, getUtf8 ]
    imports [ fx.Effect, pf.Task.{ Task } ]

Url : Str

HttpErr a : [ Status U16 Url ]a

HttpGetUtf8Err a : [ HttpGetUtf8Err (HttpErr [ BadUtf8 Url ]) ]a


getUtf8 : Url -> Task Str (HttpGetUtf8Err *)
getUtf8 = \url ->
    Effect.map (Effect.httpGetUtf8 url) \resp ->
        helper url resp
            |> Result.mapErr HttpGetUtf8Err


helper : Url, { status : U16, body : Str } -> Result Str (HttpErr [ BadUtf8 Url ]*)
helper = \url, { status, body } ->
    when status is
        200 -> Ok body
        204 -> Ok body
        _ -> Err (Status status url)
