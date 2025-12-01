app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr

str : Str -> Str
str = |s| s

main! = || {
    x = List.get([], 0)
    if Try.is_ok(x) {
        Stdout.line!("is ok")
    } else {
        Stdout.line!("is err")
    }
}
