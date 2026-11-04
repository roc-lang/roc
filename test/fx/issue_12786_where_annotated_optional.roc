app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Opt := [O].{
    persist : Opt, { id ?: U64 } -> U64
    persist = |_, r|
        match r.?id {
            Ok(id) => id + 1
            Err(MissingField) => 0
        }
}

create = |x, n| x.persist({ id: n })

outer : a, U64 -> U64 where [a.persist : a, { id ?: U64 } -> U64]
outer = |x, n| create(x, n)

main! = || Stdout.line!(outer(Opt.O, 1).to_str())
