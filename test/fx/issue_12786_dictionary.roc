app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Holder := [H].{
    make = |_self, v| { a: v }
}

use = |x, v| x.make(v)

main! = || {
    r : { a : U64 }
    r = use(Holder.H, 1.U64)
    o : { a ?: U64 }
    o = use(Holder.H, 2.U64)
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
