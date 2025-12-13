app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Reproducer for issue 8666:
# Compiler panics when accessing list elements with inferred index types in for loops

main! = || {
    list = [""]
    indices = [0]
    for i in indices {
        _x = List.get(list, i)
    }
    Stdout.line!("ok")
}
