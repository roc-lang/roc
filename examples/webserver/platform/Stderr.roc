interface Stderr
    exposes [line, write]
    imports [Effect, Task.{ Task }, InternalTask]

## Write the given string to [standard error](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)),
## followed by a newline.
##
## > To write to `stderr` without the newline, see [Stderr.write].
line : Str -> Task {} *
line = \str ->
    Effect.stderrLine str
    |> Effect.map (\_ -> Ok {})
    |> InternalTask.fromEffect

## Write the given string to [standard error](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)).
##
## Most terminals will not actually display strings that are written to them until they receive a newline,
## so this may appear to do nothing until you write a newline!
##
## > To write to `stderr` with a newline at the end, see [Stderr.line].
write : Str -> Task {} *
write = \str ->
    Effect.stderrWrite str
    |> Effect.map (\_ -> Ok {})
    |> InternalTask.fromEffect
