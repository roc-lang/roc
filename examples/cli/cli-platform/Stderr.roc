interface Stderr
    exposes [line, write]
    imports [Effect, Task.{ Task }, InternalTask]

line : Str -> Task {} * [Write [Stderr]]
line = \str ->
    Effect.stderrLine str
    |> Effect.map (\_ -> Ok {})
    |> InternalTask.fromEffect

write : Str -> Task {} * [Write [Stderr]]
write = \str ->
    Effect.stderrWrite str
    |> Effect.map (\_ -> Ok {})
    |> InternalTask.fromEffect
