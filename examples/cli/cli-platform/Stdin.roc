interface Stdin
    exposes [line]
    imports [Effect, Task.{ Task }, InternalTask]

line : Task Str *
line =
    Effect.stdinLine
    |> Effect.map Ok
    |> InternalTask.fromEffect
