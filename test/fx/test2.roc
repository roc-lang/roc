app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    x = 42
    y = x
    Stdout.line!("done")
}
