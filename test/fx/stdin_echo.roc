app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

str : Str -> Str
str = |s| s

main! = || {
    line = Stdin.line!()
    Stdout.line!(str(line))
}
