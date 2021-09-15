interface Http
    exposes [ Url, HttpErr, getUtf8 ]
    imports [ fx.Effect, Task.{ Task } ]

Url : Str

HttpErr a :
    [
        HttpErr
            [
                NotFound,
                BadGateway,
                # etc...
                Unknown
            ]
    ]a

getUtf8 : Url -> Task Str [ Foo ]* #(HttpErr [ BadUtf8 ]*)
getUtf8 = \url ->
    Task.succeed ""
    # Effect.map (Effect.httpGetUtf8 url) \{ status, body } ->
    #     when status is
    #         200 -> Ok body
    #         204 -> Ok body
    #         404 -> Err (HttpErr NotFound)
    #         502 -> Err (HttpErr BadGateway)
    #         # etc...
    #         _ -> Err (HttpErr Unknown)
