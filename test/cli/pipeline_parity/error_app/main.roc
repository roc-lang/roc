app [main!] {
    pf: platform "../../../fx/platform/main.roc",
    alpha: "../pkg_alpha/main.roc",
}

import pf.Stdout
import alpha.Alpha

main! = || {
    mistyped : Str
    mistyped = 42
    Stdout.line!(Alpha.greet(mistyped))
}
