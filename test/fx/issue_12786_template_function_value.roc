app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

mk = |v| { a: v }

main! = || {
    r : List({ a : U64 })
    r = List.map([1.U64, 2], mk)
    o : List({ a ?: U64 })
    o = List.map([3.U64], mk)
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
