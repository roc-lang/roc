## The standard input ([`stdin`](https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin))) [I/O stream](https://en.wikipedia.org/wiki/Standard_streams).
interface Stdin
    exposes [ ln, readUntil, readUntilEof, write, stream ]
    imports [ fx.Effect, Task.{ Task } ]

## Lock `stdin` and read from it until encountering `"\n"`.
## Discards the `"\n"` and returns whatever else was read.
##
## Note that the task will not complete until a `"\n"` is encountered,
## so it might never complete!
##
## This does the same thing as calling [readUntil] passing `"\n"`.
ln : Task Str [ BadUtf8 ]Io.Err
ln = Effect.map Effect.readLine Ok

## Lock `stdin` and read from it until encountering the given string.
## Discards the given string and returns whatever else was read.
##
## Note that the task will not complete until the given string is encountered,
## so it might never complete!
readUntil : Str -> Task Str [ BadUtf8 ]Io.Err
readUntil = \str ->
    Effect.map (Effect.readUntil str) Ok

## Lock `stdin` and read from it until the user sends an EOF signal (typically
## by pressing Ctrl-D in UNIX terminals, or Ctrl-Z followed by Enter in Windows
## terminals).
##
## Note that the task will not complete until an EOF signal is sent to `stdin`,
## so it might never complete!
readUntilEof : Task Str [ BadUtf8 ]Io.Err
readUntilEof = \str ->
    Effect.map (Effect.readUntilEof str) Ok

## Open a `stdin` stream.
##
## Note that in Windows, attempting to write bytes to `stdin` that are not
## valid UTF-8 will result in an error.
stream : Task Stream Io.Err
stream =
    Effect.openStdin
        |> Effect.map (\raw -> Ok (Stream.fromRaw raw))
