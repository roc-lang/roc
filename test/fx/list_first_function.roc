app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    _s = List.first([1])
    Stdout.line!("ok")
}
