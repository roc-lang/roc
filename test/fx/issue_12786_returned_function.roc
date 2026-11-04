app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

make = |n| { f: |_| { id: n } }

main! = || {
    r = make(1.U64)
    v : { id : U64 }
    v = (r.f)({})
    Stdout.line!(v.id.to_str())
}
