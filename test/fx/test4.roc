app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    x = "hello"
    y = "${x}"
    Stdout.line!(y)
}
