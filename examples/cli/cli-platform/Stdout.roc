interface Stdout
    exposes [line, write]
    imports [Effect, Task.{ Task }, InternalTask]

line : Str -> Task {} *
line = \str ->
    Effect.stdoutLine str
    |> Effect.map (\_ -> Ok {})
    |> InternalTask.fromEffect

write : Str -> Task {} *
write = \str ->
    Effect.stdoutWrite str
    |> Effect.map (\_ -> Ok {})
    |> InternalTask.fromEffect
