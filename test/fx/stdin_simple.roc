app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stderr

str : Str -> Str
str = |s| s

main! = || {
    line = Stdin.line!()
    Stderr.line!(str(line))
}
