# Regression for https://github.com/roc-lang/roc/issues/10154
app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdin

call! = |effect!| effect!()

main! = || {
    _line = call!(Stdin.line!)
}
