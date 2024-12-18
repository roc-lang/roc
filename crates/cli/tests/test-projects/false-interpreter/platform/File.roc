module [line!, withOpen!, chunk!, Handle]

import pf.Host

Handle := U64

line! : Handle => Str
line! = \@Handle handle ->
    Host.getFileLine! handle

chunk! : Handle => List U8
chunk! = \@Handle handle ->
    Host.getFileBytes! handle

open! : Str => Handle
open! = \path ->
    Host.openFile! path
    |> @Handle

close! : Handle => {}
close! = \@Handle handle ->
    Host.closeFile! handle

withOpen! : Str, (Handle => a) => a
withOpen! = \path, callback! ->
    handle = open! path
    result = callback! handle
    close! handle

    result
