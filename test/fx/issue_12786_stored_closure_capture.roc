app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

make = |n| |_| { a: n, b: "stored" }

wrap = |f| |x| f(x)

required : {} -> { a : Str, b : Str }
required = wrap(make("r"))

optional : {} -> { a ?: Str, b : Str }
optional = wrap(make("o"))

main! = || {
    r = required({})
    o = optional({})
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
