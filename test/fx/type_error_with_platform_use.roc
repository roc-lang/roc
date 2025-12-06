app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr

str : Str -> Str
str = |s| NotAString

main! = || {
    Stdout.line!(str("Hello from stdout!"))
}
