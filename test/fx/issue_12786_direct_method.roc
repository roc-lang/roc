app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Holder := [H].{
    make = |_self, v| { a: v }
}

main! = || {
    r : { a : U64 }
    r = Holder.H.make(1.U64)
    o : { a ?: U64 }
    o = Holder.H.make(2.U64)
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
