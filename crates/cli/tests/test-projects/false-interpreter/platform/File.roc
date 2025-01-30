module [line!, with_open!, chunk!, Handle]

import pf.Host

Handle := U64

line! : Handle => Str
line! = \@Handle(handle) ->
    Host.get_file_line!(handle)

chunk! : Handle => List U8
chunk! = \@Handle(handle) ->
    Host.get_file_bytes!(handle)

open! : Str => Handle
open! = \path ->
    Host.open_file!(path)
    |> @Handle

close! : Handle => {}
close! = \@Handle(handle) ->
    Host.close_file!(handle)

with_open! : Str, (Handle => a) => a
with_open! = \path, callback! ->
    handle = open!(path)
    result = callback!(handle)
    close!(handle)

    result
