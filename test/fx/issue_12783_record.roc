app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    s = Json.to_str_try({ n: 1.U64 }) ?? "failed"
    Stdout.line!(s)
}
