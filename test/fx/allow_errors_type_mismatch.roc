app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    x : U8
    x = 1
    Stdout.line!(x)
}
