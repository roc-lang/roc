app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list = [1]
    Stdout.line!("List created")

    # Try qualified call first (works)
    Stdout.line!("Calling List.fold_rev (qualified)")
    r1 = List.fold_rev(list, 0, |elem, acc| elem + acc)
    Stdout.line!("Qualified result: ${r1.to_str()}")

    # Now try method call (crashes)
    Stdout.line!("Calling list.fold_rev (method)")
    r2 = list.fold_rev(0, |elem, acc| elem + acc)
    Stdout.line!("Method result: ${r2.to_str()}")
}
