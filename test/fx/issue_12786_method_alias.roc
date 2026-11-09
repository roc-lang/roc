app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

helper = |_self, v| { a: v }

Foo := [F].{
    make = helper
}

main! = || {
    r : { a : U64 }
    r = Foo.F.make(1.U64)
    o : { a ?: U64 }
    o = Foo.F.make(2.U64)
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
