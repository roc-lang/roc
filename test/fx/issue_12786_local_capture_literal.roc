app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

run = |n| {
    mk = |v| { a: v, b: n }
    r : { a : U64, b : U64 }
    r = mk(1)
    o : { a ?: U64, b : U64 }
    o = mk(2)
    (r, o)
}

main! = || {
    pair = run(9.U64)
    Stdout.line!(Str.inspect(pair.0))
    Stdout.line!(Str.inspect(pair.1))
}
