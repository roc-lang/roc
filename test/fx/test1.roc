app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    x = 42
    Stdout.line!("done")
}
