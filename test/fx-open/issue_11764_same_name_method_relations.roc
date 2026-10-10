app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11764
import pf.Stdout

Conn :: {}.{
    ping : Conn -> Try({}, [Closed])
    ping = |_conn| Err(Closed)
}

Source :: { available : Bool }.{
    acquire! : Source => Try(Conn, [TimedOut])
    acquire! = |source| if source.available Ok(Conn.{}) else Err(TimedOut)
}

Pool(source) :: { source : source }.{
    # `ping` is called twice on one receiver here: once below with its error
    # replaced, and once through `body!` in `query!`. Only the first call's
    # relation binds the error row of `ping` that `? |_| Busy` discards.
    with! = |pool, body!| {
        conn = pool.source.acquire!()?
        conn.ping() ? |_| Busy
        body!(conn)
    }

    query! : Pool(_) => Try({}, _)
    query! = |pool| pool.with!(|conn| conn.ping())
}

# Unannotated, so `query!` is selected through this function's evidence
# rather than through a checked call site.
run! = |pool| pool.query!()

main! = |args| {
    # The platform includes argv[0]; no additional args acquires a connection.
    source = Source.{ available: List.len(args) == 1 }
    match run!(Pool.{ source }) {
        Ok({}) => Stdout.line!("ok")
        Err(Busy) => Stdout.line!("busy")
        Err(Closed) => Stdout.line!("closed")
        Err(TimedOut) => Stdout.line!("timed out")
    }
    Ok({})
}
