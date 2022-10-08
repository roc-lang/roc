interface Stdout
    exposes [line]
    imports [Effect, Task.{ Task }, InternalTask]

line : Str -> Task {} * [Write [Stdout]*]*
line = \str ->
    Effect.map (Effect.stdoutLine str) (\_ -> Ok {})
    |> InternalTask.fromEffect
