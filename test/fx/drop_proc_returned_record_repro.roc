app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    make = |a, b| {
        value: { a, b },
        help: Str.concat("  --a <value>", "  --b <value>"),
    }

    _ = make("1", "2")
    Stdout.line!("done")
}
