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
    (create(Req.R), create(Opt.O))
}

main! = || {
    pair = run(1.U64)
    Stdout.line!(pair.0.to_str())
    Stdout.line!(pair.1.to_str())
}
