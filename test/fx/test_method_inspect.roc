app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    x = "hello"
    Stdout.line!(x.inspect())
}
