interface Stdout
    exposes [ line ]
    imports [ fx.Effect, Task.{ Task } ]

# line : Str -> Task.Task {} *
# line = \line -> Effect.map (Effect.putLine line) (\_ -> Ok {})
line : Str -> Task {} *
line = \str -> Effect.map (Effect.putLine str) (\_ -> Ok {})
