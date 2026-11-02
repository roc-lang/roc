app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    mk = |v| { a: v }
    r : { a : U64 }
    r = mk(1.U64)
    o : { a ?: Str }
    o = mk("x")
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
