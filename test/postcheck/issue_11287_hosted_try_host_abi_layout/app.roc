app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = |_args| {
    Stdout.report!(Err(StdoutErr("boom")))
    Ok({})
}
