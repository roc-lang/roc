app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

identity : a -> a
identity = |x| x

main! = || {
    str = identity("Hello")
    Stdout.line!(str)
}
