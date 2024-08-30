module [line, withOpen, chunk, Handle]

import pf.PlatformTasks

Handle := U64

line : Handle -> Task Str *
line = \@Handle handle ->
    PlatformTasks.getFileLine handle
    |> Task.mapErr \_ -> crash "unreachable File.line"

chunk : Handle -> Task (List U8) *
chunk = \@Handle handle ->
    PlatformTasks.getFileBytes handle
    |> Task.mapErr \_ -> crash "unreachable File.chunk"

open : Str -> Task Handle *
open = \path ->
    PlatformTasks.openFile path
    |> Task.mapErr \_ -> crash "unreachable File.open"
    |> Task.map @Handle

close : Handle -> Task.Task {} *
close = \@Handle handle ->
    PlatformTasks.closeFile handle
    |> Task.mapErr \_ -> crash "unreachable File.close"

withOpen : Str, (Handle -> Task {} a) -> Task {} a
withOpen = \path, callback ->
    handle = open! path
    result = callback handle |> Task.result!
    close! handle

    Task.fromResult result
