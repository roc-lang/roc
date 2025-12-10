app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!("Testing fold (not fold_rev) method")
    r = [1, 2, 3].fold(0, |acc, elem| acc + elem)
    Stdout.line!("fold method result: ${r.to_str()}")
}
