interface Stdin
    exposes [line]
    imports [Effect, Task.{ Task }, InternalTask]

line : Task Str * [Read [Stdin]*]*
line =
    Effect.getLine
    |> Effect.map Ok
    |> InternalTask.fromEffect
