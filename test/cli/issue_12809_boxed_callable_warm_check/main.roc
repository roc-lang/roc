# Repro for https://github.com/roc-lang/roc/issues/12809: `view` holds a
# boxed function behind two nominal types declared in imported modules. A
# warm check after an edit to this file alone restores both modules from the
# cache and must evaluate `view` again without failing.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import Attribute
import Html
import pf.Stdout

view : Html
view = Html.input(Attribute.on_input(|v| v))

main! = || Stdout.line!(Html.render(view))
