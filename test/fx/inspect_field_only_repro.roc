app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    rec = { name: "test", count: 42 }
    Stdout.line!(rec.name)
}
