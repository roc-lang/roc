interface Http
    exposes [ getStr ]
    imports [ fx.Effect, Task ] # TODO FIXME Task.{ Task }

getStr : Str -> Task.Task {} *
getStr = \url -> Effect.map (Effect.httpGetStr url) (\_ -> Ok {})
