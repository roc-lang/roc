app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

mk = |v| { outer: { inner: v } }

main! = || {
    r : { outer : { inner : U64 } }
    r = mk(1.U64)
    o : { outer ?: { inner ?: U64 } }
    o = mk(2.U64)
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
