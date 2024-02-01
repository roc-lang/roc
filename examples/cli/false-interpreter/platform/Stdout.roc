interface Stdout
    exposes [line, raw]

import pf.Effect
import Task exposing [Task]

line : Str -> Task {} *
line = \str -> Effect.map (Effect.putLine str) (\_ -> Ok {})

raw : Str -> Task {} *
raw = \str -> Effect.map (Effect.putRaw str) (\_ -> Ok {})
