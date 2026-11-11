# Repro for https://github.com/roc-lang/roc/issues/12859.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Color

unused_color = Color.make(0)

main! = |_args| {
    Stdout.line!(Str.inspect(Color.blue))
    Ok({})
}
