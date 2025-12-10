app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test that fold_rev works with qualified call
# This should work fine
main! = || {
    Stdout.line!("Start reverse (qualified)")
    rev = List.fold_rev([1, 2, 3], [], |elem, acc| {
        acc.append(elem)
    })
    # rev should be [3, 2, 1]
    Stdout.line!("Reversed: ${rev.len().to_str()} elements")
    Stdout.line!("Done")
}
