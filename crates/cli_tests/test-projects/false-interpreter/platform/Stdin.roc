module [
    line,
    char,
]

import pf.PlatformTasks

line : {} -> Task Str *
line = \{} ->
    PlatformTasks.getLine
    |> Task.mapErr \_ -> crash "unreachable Stdin.line"

char : {} -> Task U8 *
char = \{} ->
    PlatformTasks.getChar
    |> Task.mapErr \_ -> crash "unreachable Stdin.char"
