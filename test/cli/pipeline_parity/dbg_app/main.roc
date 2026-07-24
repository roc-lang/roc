app [main!] { pf: platform "../../../fx/platform/main.roc" }

import pf.Stdout

main! = || {
    dbg "ninety-four"
    Stdout.line!("done 9694")
}
