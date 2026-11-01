## A record-wrapped host handle shaped like basic-cli's `Cmd.Child`.
Proc :: { host : Box(U64), pid_value : U32, command : Str }.{

    ## Create a host-tracked handle holding the given value.
    handle_new! : U64 => Box(U64)

    ## Read a live handle's value as `Ok`, releasing the owned argument.
    handle_poll! : Box(U64) => Try(U64, [HostErr(Str)])

    ## Check a live handle as `Ok`, releasing the owned argument.
    handle_close! : Box(U64) => Try({}, [HostErr(Str)])

    ## Read a live handle's value plus the two scalars as `Ok`, releasing the
    ## owned argument.
    handle_read_exactly! : Box(U64), U64, U64 => Try(U64, [HostErr(Str)])

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

    ## A host handle that is the record's only field, shaped like basic-cli's
    ## `File.Reader`.
    Reader :: { host : Box(U64) }.{

        open! : U64 => Reader
        open! = |value| { host: Proc.handle_new!(value) }

        ## Read the handle, rebuilding each result, as `File.Reader.read_up_to!`
        ## does.
        read! : Reader => Try(U64, [EndOfInput, ReadErr(Str)])
        read! = |reader|
            match Proc.handle_poll!(reader.host) {
                Ok(n) => Ok(n)
                Err(HostErr(e)) => Err(ReadErr(e))
            }

        ## Read the handle, forwarding through `map_err`, as `File.Reader.seek!`
        ## does.
        peek! : Reader => Try(U64, [ReadErr(Str)])
        peek! = |reader|
            Proc.handle_poll!(reader.host)
                .map_err(|HostErr(e)| ReadErr(e))
    }

    ## A host handle that is the record's only refcounted field.
    Counted :: { host : Box(U64), reads : U32 }.{

        open! : U64 => Counted
        open! = |value| { host: Proc.handle_new!(value), reads: 0 }

        read! : Counted => Try(U64, [EndOfInput, ReadErr(Str)])
        read! = |counted|
            match Proc.handle_poll!(counted.host) {
                Ok(n) => Ok(n)
                Err(HostErr(e)) => Err(ReadErr(e))
            }

        peek! : Counted => Try(U64, [ReadErr(Str)])
        peek! = |counted|
            Proc.handle_poll!(counted.host)
                .map_err(|HostErr(e)| ReadErr(e))
    }

    ## A timeout read through a method, shaped like basic-cli's `Duration`.
    Timeout :: { nanos : U64 }.{

        to_nanos : Timeout -> U64
        to_nanos = |timeout| timeout.nanos
    }

    ## A host handle beside scalar settings, shaped like basic-cli's
    ## `Tcp.Connection`.
    Connection :: { host : Box(U64), timeout : Timeout, line_limit : U64 }.{

        open! : U64 => Connection
        open! = |value| { host: Proc.handle_new!(value), timeout: { nanos: 10 }, line_limit: 100 }

        ## Read the handle, rebuilding each result, as
        ## `Tcp.Connection.read_exactly!` does.
        read_exactly! : Connection, U64 => Try(U64, [UnexpectedEndOfInput, ReadErr(Str)])
        read_exactly! = |conn, count|
            match Proc.handle_read_exactly!(conn.host, count, conn.timeout.to_nanos()) {
                Ok(n) => Ok(n)
                Err(HostErr(e)) => Err(ReadErr(e))
            }

        ## The later read a caller makes, forwarding through `map_err`.
        peek! : Connection => Try(U64, [ReadErr(Str)])
        peek! = |conn|
            Proc.handle_poll!(conn.host)
                .map_err(|HostErr(e)| ReadErr(e))
    }

    ## A host handle that is the payload of a single-tag union.
    Tagged :: [Tagged(Box(U64))].{

        open! : U64 => Tagged
        open! = |value| Tagged(Proc.handle_new!(value))

        read! : Tagged => Try(U64, [EndOfInput, ReadErr(Str)])
        read! = |tagged| {
            Tagged(host) = tagged
            match Proc.handle_poll!(host) {
                Ok(n) => Ok(n)
                Err(HostErr(e)) => Err(ReadErr(e))
            }
        }

        peek! : Tagged => Try(U64, [ReadErr(Str)])
        peek! = |tagged| {
            Tagged(host) = tagged
            Proc.handle_poll!(host)
                .map_err(|HostErr(e)| ReadErr(e))
        }
    }
}
