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
