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

helper = |_self, x, n| x.persist({ id: n })

Foo := [F].{
    go = helper
}

main! = || {
    Stdout.line!(Foo.F.go(Req.R, 1.U64).to_str())
    Stdout.line!(Foo.F.go(Opt.O, 1.U64).to_str())
}
