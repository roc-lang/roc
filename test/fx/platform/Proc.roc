## A record-wrapped host handle shaped like basic-cli's `Cmd.Child`.
Proc :: { host : Box(U64), pid_value : U32, command : Str }.{

    ## Create a host-tracked handle holding the given value.
    handle_new! : U64 => Box(U64)

    ## Read a live handle's value as `Ok`, releasing the owned argument.
    handle_poll! : Box(U64) => Try(U64, [HostErr(Str)])

    ## Check a live handle as `Ok`, releasing the owned argument.
    handle_close! : Box(U64) => Try({}, [HostErr(Str)])

    spawn! : Str => Try(Proc, [SpawnErr(Str)])
    spawn! = |command| Ok({ host: Proc.handle_new!(7), pid_value: 1, command })

    poll! : Proc => Try(U64, [PollErr(Str), Closed])
    poll! = |proc|
        match Proc.handle_poll!(proc.host) {
            Ok(n) => Ok(n)
            Err(HostErr(e)) => Err(PollErr(Str.concat(e, proc.command)))
        }

    close! : Proc => Try({}, [CloseErr(Str)])
    close! = |proc|
        match Proc.handle_close!(proc.host) {
            Ok({}) => Ok({})
            Err(HostErr(e)) => Err(CloseErr(Str.concat(e, proc.command)))
        }
}
