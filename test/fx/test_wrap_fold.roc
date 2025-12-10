app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Wrap fold_rev to test if the issue is with method dispatch
my_fold_rev = |list, init, step| List.fold_rev(list, init, step)

main! = || {
    list = [1]
    Stdout.line!("List created")
    
    # Call our wrapper (which uses qualified internally)
    Stdout.line!("Calling my_fold_rev")
    r = my_fold_rev(list, 0, |elem, acc| elem + acc)
    Stdout.line!("Result: ${r.to_str()}")
}
