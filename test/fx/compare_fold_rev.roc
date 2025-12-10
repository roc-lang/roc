app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!("Testing qualified fold_rev")
    r1 = List.fold_rev([1], 0, |elem, acc| acc + elem)
    Stdout.line!("Qualified result: ${r1.to_str()}")
    
    Stdout.line!("Testing method fold_rev")
    r2 = [1].fold_rev(0, |elem, acc| acc + elem)
    Stdout.line!("Method result: ${r2.to_str()}")
}
