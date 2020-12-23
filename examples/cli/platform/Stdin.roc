interface Stdin
    exposes [ line ]
    imports [ Effect, Task ]

line : Task.Task Str *
line = Effect.after Effect.getLine Task.always # TODO FIXME Effect.getLine should suffice
