interface Http
    exposes [HttpErr, getUtf8]
    imports [Effect, Url.{ Url }, Task.{ Task }, InternalTask]

HttpErr a : [
    HttpGetErr [
        NotFound Url,
        Timeout Url,
    ]
]a

getUtf8 : Url -> Task Str (HttpErr *) [Net]*
getUtf8 = \url ->
    Effect.httpGetUtf8 url
    |> Effect.map Ok # TODO actually handle errors
    |> InternalTask.fromEffect
