app [main!] {
    pf: platform "../../../fx/platform/main.roc",
    alpha: "../pkg_alpha/main.roc",
}

import pf.Stdout
import alpha.Alpha

main! = || {
    Stdout.line!(Alpha.greet("parity"))
}

expect Alpha.greet("x") == "alpha[beta:x]"
