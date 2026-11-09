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

outer : a, U64 -> List(U64) where [a.persist : a, { id ?: U64 } -> U64]
outer = |x, n| {
    create = |y| y.persist({ id: n })
    List.map([x], create)
}

main! = || Stdout.line!(Str.inspect(outer(Opt.O, 1)))
