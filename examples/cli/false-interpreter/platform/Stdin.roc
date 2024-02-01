interface Stdin
    exposes [char]

import pf.Effect
import Task

# line : Task.Task Str *
# line = Effect.after Effect.getLine Task.succeed # TODO FIXME Effect.getLine should suffice
char : Task.Task U8 *
char = Effect.after Effect.getChar Task.succeed # TODO FIXME Effect.getLine should suffice
