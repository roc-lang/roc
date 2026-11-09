app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

mk = |v, s| { a: v, b: s }

main! = || {
    s = Str.repeat("a string long enough to live on the heap ", 2)
    r : { a : Str, b : List(Str) }
    r = mk(s, [s])
    o : { a ?: Str, b : List(Str) }
    o = mk(s, [s, s])
    Stdout.line!(Str.inspect(r.b.len()))
    Stdout.line!(Str.inspect(o.b.len()))
}
