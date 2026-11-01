app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

helper = |_self, x, n| {
    _ = Json.to_str(x)
    n + 1
}

Foo := [F].{
    go = helper
}

outer = |y| {
    r : U64
    r = Foo.F.go({ w: y }, 0)
    r
}

main! = || Stdout.line!(outer(1.U64).to_str())
