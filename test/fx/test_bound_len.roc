app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

my_len = |list| list.len()

main! = || {
    Stdout.line!("Testing bound list len")
    list = [1, 2, 3]
    r = my_len(list)
    Stdout.line!("Result: ${r.to_str()}")
}
