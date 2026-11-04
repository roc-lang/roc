app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Req := [R].{
    persist : Req, { id : U64 } -> U64
    persist = |_, r| r.id
}

Opt := [O].{
    persist : Opt, { id ?: U64 } -> U64
    persist = |_, r|
        match r.?id {
            Ok(id) => id + 1
            Err(MissingField) => 0
        }
}

run = |n| {
    create = |x| x.persist({ id: n })
    (List.map([Req.R], create), List.map([Opt.O], create))
}

main! = || {
    pair = run(1.U64)
    Stdout.line!(Str.inspect(pair.0))
    Stdout.line!(Str.inspect(pair.1))
}
