## The standard output ([`stdout`](https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout))) [I/O stream](https://en.wikipedia.org/wiki/Standard_streams).
interface Stdout
    exposes [ ln, write, stream ]
    imports [ fx.Effect, Task.{ Task } ]

## Lock `stdout` and write a line (a string followed by a newline) to it.
ln : Str -> Task {} Io.Err
ln = \str -> Effect.map (Effect.putLine str) (\_ -> Ok {})

## Lock `stdout` and write a string to it.
##
## To write a line instead (with a newline at the end), see [ln].
write : Str -> Task {} Io.Err
write = \str -> Effect.map (Effect.putLine str) (\_ -> Ok {})

## Open a `stdout` stream.
##
## Note that in Windows, attempting to write bytes to `stdout` that are not
## valid UTF-8 will result in an error.
stream : Task Stream Io.Err
stream =
    Effect.openStdout
        |> Effect.map (\raw -> Ok (Stream.fromRaw raw))
