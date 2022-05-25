interface Stdout
    exposes [line, raw]
    imports [pf.Effect, Task.{ Task }]

line : Str -> Task {} *
line = \str -> Effect.map (Effect.putLine str) (\_ -> Ok {})

raw : Str -> Task {} *
raw = \str -> Effect.map (Effect.putRaw str) (\_ -> Ok {})
