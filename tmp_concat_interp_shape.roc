app [main!] { pf: platform "./test/fx/platform/main.roc" }

import pf.Stdout

main! = || {
    msg = "Red"
    out = Str.concat(Str.concat("My favourite color is ", msg), "")
    Stdout.line!(out)
}
