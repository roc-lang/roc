interface Http
    exposes [ getUtf8 ]
    imports [ fx.Effect, Task ] # TODO FIXME Task.{ Task }

getUtf8 : Str -> Task.Task {} *
getUtf8 = \url -> Effect.map (Effect.httpGetUtf8 url) (\_ -> Ok {})
