app [main!] {
    pf: platform "../../../fx/platform/main.roc",
    alpha: "../pkg_alpha/main.roc",
}

import pf.Stdout
import alpha.Alpha

main! = || {
    unused_parity_var = 1
    Stdout.line!(Alpha.greet("warn"))
}

expect Alpha.greet("w") == "alpha[beta:w]"
