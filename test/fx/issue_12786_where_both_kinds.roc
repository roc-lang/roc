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

create = |x, n| x.persist({ id: n })

outer = |x, n| create(x, n)

main! = || {
    Stdout.line!(outer(Req.R, 1.U64).to_str())
    Stdout.line!(outer(Opt.O, 1.U64).to_str())
}
