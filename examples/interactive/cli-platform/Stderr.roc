interface Stderr
    exposes [line]
    imports [Effect, Task.{ Task }, InternalTask]

line : Str -> Task {} * [Write [Stdout]*]*
line = \str ->
    Effect.map (Effect.errLine str) (\_ -> Ok {})
    |> InternalTask.fromEffect
