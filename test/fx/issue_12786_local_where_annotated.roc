app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Req := [R].{
    persist : Req, { id : U64 } -> U64
    persist = |_, r| r.id
}

outer : a, U64 -> U64 where [a.persist : a, { id : U64 } -> U64]
outer = |x, n| {
    create = |y, m| y.persist({ id: m })
    create(x, n)
}

main! = || Stdout.line!(outer(Req.R, 1).to_str())
