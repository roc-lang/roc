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

main! = || {
    create = |x, n| x.persist({ id: n })
    Stdout.line!(create(Req.R, 1.U64).to_str())
    Stdout.line!(create(Opt.O, 1.U64).to_str())
}
