app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

persist : { id : U64 } -> U64
persist = |r| r.id

create = |h, n| match h {
    Holder(f) => f({ id: n })
}

main! = || Stdout.line!(create(Holder(persist), 1.U64).to_str())
