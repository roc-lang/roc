interface Stderr
    exposes [ line ]
    imports [ fx.Effect, Task.{ Task } ]

line : Str -> Task {} *
line = \str -> Effect.map (Effect.errLine str) (\_ -> Ok {})
