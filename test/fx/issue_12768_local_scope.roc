app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

outer = |y| {
    inner = |x, n| {
        _ = Json.to_str(x)
        _ = Json.to_str(y)
        n + 1
    }
    r : U64
    r = inner({ w: y }, 0)
    r
}

main! = || Stdout.line!(outer(1.U64).to_str())
