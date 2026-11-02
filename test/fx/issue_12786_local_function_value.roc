app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

run = |xs, ys| {
    mk = |v| { a: v }
    r : List({ a : U64 })
    r = List.map(xs, mk)
    o : List({ a ?: Str })
    o = List.map(ys, mk)
    (r, o)
}

main! = || {
    pair = run([1.U64, 2], ["x"])
    Stdout.line!(Str.inspect(pair.0))
    Stdout.line!(Str.inspect(pair.1))
}
