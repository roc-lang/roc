interface Stdout
    exposes [ line ]
    imports [ pf.Effect, Task.{ Task } ]

line : Str -> Task {} *
line = \str -> Effect.map (Effect.putLine str) (\_ -> Ok {})
