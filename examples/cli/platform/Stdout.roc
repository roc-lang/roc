interface Stdout
    exposes [ line ]
    imports [ fx.Effect, Task ]

line : Str -> Task.Task {} *
line = \line -> Effect.map (Effect.putLine line) (\_ -> Ok {})
