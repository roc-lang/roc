app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    _ = Str.concat("  --a <value>", "  --b <value>")
    Stdout.line!("done")
}
