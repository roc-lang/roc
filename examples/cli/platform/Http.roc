interface Http
    exposes [ Url, HttpErr, HttpGetErr, get ]
    imports [ fx.Effect, pf.Task.{ Task } ]

Url : Str

HttpErr a : [ Status U16 Url ]a

HttpGetErr a : [ HttpGetErr (HttpErr [ Bad Url ]) ]a


get : Url -> Task Str (HttpGetErr *)
get = \url ->
    Effect.map (Effect.httpGet url) \resp ->
        helper url resp
            |> Result.mapErr HttpGetErr


helper : Url, { status : U16, body : Str } -> Result Str (HttpErr [ Bad Url ]*)
helper = \url, { status, body } ->
    when status is
        200 -> Ok body
        204 -> Ok body
        _ -> Err (Status status url)
