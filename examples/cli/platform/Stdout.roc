interface Stdout
    exposes [ line ]
    imports [ fx.Effect, Task ] # TODO FIXME Task.{ Task }

line : Str -> Task.Task {} *
line = \str -> Effect.map (Effect.putLine str) (\_ -> Ok {})
