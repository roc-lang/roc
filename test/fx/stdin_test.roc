app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stdin

str : Str -> Str
str = |s| s

main! = || {
    Stdout.line!(str("Before stdin"))
    temp = Stdin.line!()
    Stdout.line!(str("After stdin"))
}
