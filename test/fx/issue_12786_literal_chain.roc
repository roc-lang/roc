app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

inner = |n| { a: n + 1 }

wrap = |n| inner(n)

main! = || {
    r : { a : U64 }
    r = wrap(1)
    o : { a ?: U64 }
    o = wrap(2)
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
