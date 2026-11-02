app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

mk = |v| { a: v }

table = { f: mk }

main! = || {
    r : { a : U64 }
    r = (table.f)(1.U64)
    Stdout.line!(Str.inspect(r))
}
