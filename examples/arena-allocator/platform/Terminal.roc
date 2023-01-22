interface Terminal
    exposes [ readLine, write ]
    imports [ fx.Effect, Task.{ Task } ]

readLine : Task Str *
readLine =
    Effect.after Effect.stdinRead Task.succeed

write : Str -> Task {} *
write = \str ->
    Effect.after (Effect.stdoutWrite str) Task.succeed
