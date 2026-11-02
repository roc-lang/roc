app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

mk = |v| [{ a: v }, { a: v }]

main! = || {
    r : List({ a : U64 })
    r = mk(1.U64)
    o : List({ a ?: U64 })
    o = mk(2.U64)
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
