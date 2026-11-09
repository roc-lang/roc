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

make = |x| |n| x.persist({ id: n })

required : U64 -> U64
required = make(Req.R)

optional : U64 -> U64
optional = make(Opt.O)

main! = || {
    Stdout.line!(required(1).to_str())
    Stdout.line!(optional(1).to_str())
}
