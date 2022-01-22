interface Stdin
    exposes [ line ]
    imports [ fx.Effect, Task ]

line : Task.Task Str *
line = Effect.after Effect.getLine Task.succeed# TODO FIXME Effect.getLine should suffice
