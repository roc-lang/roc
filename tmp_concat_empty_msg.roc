app [main!] { pf: platform "./test/fx/platform/main.roc" }

import pf.Stdout

main! = || {
    msg = "Red"
    out = Str.concat("", msg)
    Stdout.line!(out)
}
