app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    inner = |n| { a: n + 1 }
    r : { a : U64 }
    r = inner(1)
    o : { a ?: U64 }
    o = inner(2)
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
