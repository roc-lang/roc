interface File
    exposes [ line, Handle, withOpen, open, close, chunk ]
    imports [ fx.Effect, Task.{ Task } ]

Handle: [ @Handle U64 ]

line : Handle -> Task.Task Str *
line = \@Handle handle -> Effect.after (Effect.getFileLine handle) Task.succeed

chunk : Handle -> Task.Task Str *
chunk = \@Handle handle -> Effect.after (Effect.getFileBytes handle) Task.succeed

open : Str -> Task.Task Handle *
open = \path ->
    Effect.openFile path
        |> Effect.map (\id -> @Handle id)
        |> Effect.after Task.succeed

close : Handle -> Task.Task {} *
close = \@Handle handle -> Effect.after (Effect.closeFile handle) Task.succeed

# The compiler fails to type check if I uncomment this.
# withOpen : Str, (Handle -> Task {} *) -> Task {} *
withOpen = \path, callback ->
    handle <- Task.await (open path)
    {} <- Task.await (callback handle)
    close handle