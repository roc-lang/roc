app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    x : U64
    x = 42
    _ = x
    Stdout.line!("done")
}
