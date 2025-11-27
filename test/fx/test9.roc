app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    x : U64
    x = 42
    y = x + 1
    _ = y
    Stdout.line!("done")
}
