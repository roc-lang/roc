app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    _s = [1].first()
    Stdout.line!("ok")
}
