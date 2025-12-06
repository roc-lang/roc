app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

countdown = |n|
    if n == 0 {
        0
    } else {
        countdown(n - 1)
    }

main! = || {
    _result = countdown(2)
    Stdout.line!("done")
}
