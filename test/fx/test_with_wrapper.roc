app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

str : Str -> Str
str = |s| s

main! = || {
    Stdout.line!(str("Hello"))
}
