interface Stderr
    exposes [line]
    imports [Effect, Task.{ Task }, InternalTask]

line : Str -> Task {} * [Write [Stderr]*]*
line = \str ->
    Effect.map (Effect.stderrLine str) (\_ -> Ok {})
    |> InternalTask.fromEffect
