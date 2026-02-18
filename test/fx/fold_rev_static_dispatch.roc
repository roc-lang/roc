app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test that fold_rev works with static dispatch (method syntax)
# Previously this panicked while List.fold_rev(...) worked fine
main! = || {
    Stdout.line!("Start reverse")
    rev =
        [1, 2, 3].fold_rev([], |elem, acc| {
            acc.append(elem)
        })
    # rev should be [3, 2, 1]
    Stdout.line!("Reversed: ${rev.len().to_str()} elements")
    Stdout.line!("Done")
}
