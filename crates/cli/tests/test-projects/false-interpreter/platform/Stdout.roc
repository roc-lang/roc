module [line, raw]

import pf.PlatformTasks

line : Str -> Task {} *
line = \text ->
    PlatformTasks.putLine text
    |> Task.mapErr \_ -> crash "unreachable Stdout.line"

raw : Str -> Task {} *
raw = \text ->
    PlatformTasks.putRaw text
    |> Task.mapErr \_ -> crash "unreachable Stdout.raw"
