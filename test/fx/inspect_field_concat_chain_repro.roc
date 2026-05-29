app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    rec = { name: "test", count: 42 }
    prefix = Str.concat("{ name: \"", rec.name)
    msg = Str.concat(prefix, "\", count: 42.0 }")
    Stdout.line!(msg)
}
