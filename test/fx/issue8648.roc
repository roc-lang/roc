app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    record = { x: "a", y: "b" }

    Stdout.line!(record.x)
}