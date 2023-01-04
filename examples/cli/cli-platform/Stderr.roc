interface Stderr
    exposes [line, write]
    imports [Effect, Task.{ Task }, InternalTask]

line : Str -> Task {} *
line = \str ->
    Effect.stderrLine str
    |> Effect.map (\_ -> Ok {})
    |> InternalTask.fromEffect

write : Str -> Task {} *
write = \str ->
    Effect.stderrWrite str
    |> Effect.map (\_ -> Ok {})
    |> InternalTask.fromEffect
