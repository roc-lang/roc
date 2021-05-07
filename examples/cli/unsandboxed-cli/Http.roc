interface Http
    exposes [ getUtf8 ]
    imports [ fx.Effect, Task ] # TODO FIXME Task.{ Task }

getUtf8 : Str -> Task.Task Str Str
getUtf8 = \url -> Effect.httpGetUtf8 url
