interface Http
    exposes [ Url, HttpErr, getUtf8 ]
    imports [ fx.Effect, pf.Task.{ Task } ]

Url : Str

HttpErr a :
    [
        NotFound,
        BadGateway,
        # etc...
        Unknown
    ]a


getUtf8 : Url -> Task Str [ HttpGetErr (HttpErr [ BadUtf8 ]) ]*
getUtf8 = \url ->
    (Effect.httpGetUtf8 url)
        |> Effect.map (\r ->(helper r) |> Result.mapErr HttpGetErr)


helper : { status : Int *, body : Str } -> Result Str (HttpErr [ BadUtf8 ])
helper = \{ status, body } ->
        when status is
            # 200 -> Ok body
            # 204 -> Ok body
            # 404 -> Err (NotFound)
            # 502 -> Err (BadGateway)
            # etc...
            _ -> Err (Unknown)
