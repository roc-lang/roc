module [line, withOpen, chunk, Handle]

import pf.PlatformTasks

Handle := U64

line : Handle -> Task Str *
line = \@Handle handle -> PlatformTasks.getFileLine handle

chunk : Handle -> Task (List U8) *
chunk = \@Handle handle -> PlatformTasks.getFileBytes handle

open : Str -> Task Handle *
open = \path ->
    PlatformTasks.openFile path
    |> Task.map @Handle

close : Handle -> Task.Task {} *
close = \@Handle handle -> PlatformTasks.closeFile handle

withOpen : Str, (Handle -> Task {} a) -> Task {} a
withOpen = \path, callback ->
    handle = open! path
    result = callback handle |> Task.result!
    close! handle

    Task.fromResult result
