app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

inner = |x, n| {
    _ = Json.to_str(x)
    n + 1
}

mid = |z, m| inner({ w: z }, m)

outer = |y| {
    r : U64
    r = mid(y, 0)
    r
}

main! = || Stdout.line!(outer(1.U64).to_str())
