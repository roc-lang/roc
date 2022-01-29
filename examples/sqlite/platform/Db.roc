interface Db
    exposes [ raw ]
    imports [ fx.Effect, Task.{ Task } ]

raw : Str -> Task Str *
raw = \str -> Effect.map (Effect.putLine str) (\_ -> Ok "Hi!")
