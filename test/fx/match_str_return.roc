app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

str : Str -> Str
str = |s| s

main! = || {
    x = match 0 {
        _ => str("0")
    }
    Stdout.line!(x)
}
