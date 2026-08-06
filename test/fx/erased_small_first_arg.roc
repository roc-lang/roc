app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Repro for https://github.com/roc-lang/roc/issues/10364
bump : U8, U64 -> U8
bump = |x, _ev| x + 1

main! = || {
    f = Box.unbox(Box.box(bump))

    Stdout.line!(f(1, 99).to_str())
}
