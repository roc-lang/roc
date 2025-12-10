app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list = [1]
    Stdout.line!("List created")
    
    # Simpler fold_rev without append - just sum
    Stdout.line!("Calling method fold_rev with sum")
    r = list.fold_rev(0, |elem, acc| elem + acc)
    Stdout.line!("Result: ${r.to_str()}")
}
