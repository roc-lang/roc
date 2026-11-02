app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

encode : [A(U64), B] -> Str
encode = |value| Json.to_str_try(value) ?? "failed"

main! = || Stdout.line!(encode(A(1.U64)))
