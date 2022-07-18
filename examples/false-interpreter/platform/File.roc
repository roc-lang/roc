interface File
    exposes [line, Handle, withOpen, chunk]
    imports [pf.Effect, Task.{ Task }]

Handle := U64

line : Handle -> Task.Task Str *
line = \@Handle handle -> Effect.after (Effect.getFileLine handle) Task.succeed

chunk : Handle -> Task.Task (List U8) *
chunk = \@Handle handle -> Effect.after (Effect.getFileBytes handle) Task.succeed

open : Str -> Task.Task Handle *
open = \path ->
    Effect.openFile path
    |> Effect.map (\id -> @Handle id)
    |> Effect.after Task.succeed

close : Handle -> Task.Task {} *
close = \@Handle handle -> Effect.after (Effect.closeFile handle) Task.succeed

withOpen : Str, (Handle -> Task {} a) -> Task {} a
withOpen = \path, callback ->
    handle <- Task.await (open path)
    result <- Task.attempt (callback handle)
    {} <- Task.await (close handle)
    Task.fromResult result
