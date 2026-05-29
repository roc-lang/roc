app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    rec = { name: "test", count: 42 }
    msg = Str.concat("{ name: \"", rec.name)
    Stdout.line!(msg)
}
