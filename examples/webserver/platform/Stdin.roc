interface Stdin
    exposes [
        line,
        bytes,
    ]
    imports [Effect, Task.{ Task }, InternalTask]

## Read a line from [standard input](https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin)).
##
## > This task will block the program from continuing until `stdin` receives a newline character
## (e.g. because the user pressed Enter in the terminal), so using it can result in the appearance of the
## programming having gotten stuck. It's often helpful to print a prompt first, so
## the user knows it's necessary to enter something before the program will continue.
line : Task Str *
line =
    Effect.stdinLine
    |> Effect.map Ok
    |> InternalTask.fromEffect

## Read bytes from [standard input](https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin)).
##
## > This is typically used in combintation with [Tty.enableRawMode],
## which disables defaults terminal bevahiour and allows reading input
## without buffering until Enter key is pressed.
bytes : Task (List U8) *
bytes =
    Effect.stdinBytes
    |> Effect.map Ok
    |> InternalTask.fromEffect
