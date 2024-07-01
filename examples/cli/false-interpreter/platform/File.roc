module [line, withOpen, chunk, Handle]

import pf.PlatformTask

Handle := U64

line : Handle -> Task Str *
line = \@Handle handle -> PlatformTask.getFileLine handle

chunk : Handle -> Task (List U8) *
chunk = \@Handle handle -> PlatformTask.getFileBytes handle

open : Str -> Task Handle *
open = \path ->
    PlatformTask.openFile path
    |> Task.map @Handle

close : Handle -> Task.Task {} *
close = \@Handle handle -> PlatformTask.closeFile handle

withOpen : Str, (Handle -> Task {} a) -> Task {} a
withOpen = \path, callback ->
    handle = open! path

    callback handle
    |> Task.attempt \result ->
        close! handle
        Task.fromResult result
