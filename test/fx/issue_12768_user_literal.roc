app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

inner = |x, n| {
    _ = Json.to_str(x)
    n + 1
}

Cust := { v : U64 }.{
    from_numeral : Numeral -> Try(Cust, [InvalidNumeral(Str)])
    from_numeral = |_| Ok({ v: 9 })
    plus : Cust, Cust -> Cust
    plus = |a, b| { v: a.v + b.v }
    show : Cust -> Str
    show = |c| c.v.to_str()
}

outer = |y| {
    r : Cust
    r = inner({ w: y }, 0)
    r.show()
}

main! = || Stdout.line!(outer(1.U64))
