interface Stdin
    exposes [ line ]
    imports [ pf.Effect, Task ]

line : Task.Task Str *
line = Effect.after Effect.getLine Task.succeed# TODO FIXME Effect.getLine should suffice
