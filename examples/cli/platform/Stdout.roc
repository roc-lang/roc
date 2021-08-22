interface Stdout
    exposes [ putLine, putInt ]
    imports [ fx.Effect, Task ]

putLine : Str -> Task {} *
putLine = \line -> Effect.map (Effect.putLine line) (\_ -> Ok {})

putInt : I64 -> Task {} *
putInt = \line -> Effect.map (Effect.putInt line) (\_ -> Ok {})
