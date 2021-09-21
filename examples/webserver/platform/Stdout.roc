interface Stdout
    exposes [ line ]
    imports [ fx.Effect, Task.{ Task } ]

line : Str -> Task {} *
line = \str -> Effect.map (Effect.putLine str) (\_ -> Ok {})
