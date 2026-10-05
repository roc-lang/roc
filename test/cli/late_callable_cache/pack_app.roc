app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Pack

main! = |_args| {
    Stdout.line!(Str.inspect(Pack.mapped([1, 2, 3])))
    Ok({})
}
