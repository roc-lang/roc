app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Wrap fold_rev - but can't call this via method syntax since it's in app module
# Let's test if method dispatch on a simple wrapper works
my_wrapper = |list| List.fold_rev(list, 0, |elem, acc| elem + acc)

main! = || {
    list = [1]
    Stdout.line!("List created")
    
    # Call wrapper directly
    Stdout.line!("Calling my_wrapper(list)")
    r = my_wrapper(list)
    Stdout.line!("Result: ${r.to_str()}")
}
