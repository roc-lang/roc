app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    _ = {
        help: Str.concat("  --a <value>", "  --b <value>"),
        value: { a: "1", b: "2" },
    }
    Stdout.line!("done")
}
