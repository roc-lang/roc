app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list = [1]
    Stdout.line!("List created")
    
    # Method call FIRST this time
    Stdout.line!("Calling method list.fold_rev")
    r1 = list.fold_rev(0, |elem, acc| elem + acc)
    Stdout.line!("Method result: ${r1.to_str()}")
}
