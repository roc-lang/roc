app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr

str : Str -> Str
str = |s| s

main! = || {
    Stdout.line!(str("Hello from stdout!"))
}
