app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

make = |n| |_| { a: n }

required : {} -> { a : U64 }
required = make(1.U64)

optional : {} -> { a ?: U64 }
optional = make(2.U64)

main! = || {
    r = required({})
    o = optional({})
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
