interface Stdout
    exposes [line, write]
    imports [Effect, Task.{ Task }, InternalTask]

line : Str -> Task {} * [Write [Stdout]]
line = \str ->
    Effect.stdoutLine str
    |> Effect.map (\_ -> Ok {})
    |> InternalTask.fromEffect

write : Str -> Task {} * [Write [Stdout]]
write = \str ->
    Effect.stdoutWrite str
    |> Effect.map (\_ -> Ok {})
    |> InternalTask.fromEffect
