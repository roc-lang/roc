interface Http
    exposes [ Url, HttpErr, getUtf8 ]
    imports [ fx.Effect, Task.{ Task } ]

Url : Str

HttpErr a :
    [
        NotFound,
        BadGateway,
        # etc...
        Unknown
    ]a


getUtf8 : Url -> Task Str *
getUtf8 = \url ->
    Task.succeed "TODO replace this with the commented-out version below"

# getUtf8 : Url -> Task Str [ HttpGetErr (HttpErr [ BadUtf8 ]) ]*
# getUtf8 = \url ->
#     Task.succeed ""
#     # Effect.map (Effect.httpGetUtf8 url) \{ status, body } ->
#     #     when status is
#     #         200 -> Ok body
#     #         204 -> Ok body
#     #         404 -> Err (HttpErr NotFound)
#     #         502 -> Err (HttpErr BadGateway)
#     #         # etc...
#     #         _ -> Err (HttpErr Unknown)
