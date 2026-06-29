app [main!] { pf: platform "./platform/main.roc" }

import pf.Fallible

main! : List(Str) => Try({}, [Exit(I32), LineErr(Fallible.IOErr), ..])
main! = |_args|
    # repro for https://github.com/roc-lang/roc/issues/9826
    Fallible.line!("only line")
