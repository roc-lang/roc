app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list_strs = ["2022", "22", "11", "ots"]
    _list_try = List.map(list_strs, U64.from_str)
    Stdout.line!("done")
}
