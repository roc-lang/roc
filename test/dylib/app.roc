app [main!] { pf: platform "./platform/main.roc" }

import pf.Host

main! : I64 => I64
main! = |n| {
    # A runtime list literal allocates, exercising the host's exported roc_alloc.
    list = [n, n, n]
    total = list.fold(0, |acc, elem| acc + elem)
    Host.double!(total) + 1
}
