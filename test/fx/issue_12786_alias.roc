app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

mk = |v| { a: v }
generic = mk

main! = || {
    r : { a : U64 }
    r = generic(1.U64)
    o : { a ?: U64 }
    o = generic(2.U64)
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
