app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list = [1, 2, 3]
    Stdout.line!("Method len")
    len = list.len()
    Stdout.line!("Method len: ${len.to_str()}")
    
    Stdout.line!("Qualified len")
    len2 = List.len(list)
    Stdout.line!("Qualified len: ${len2.to_str()}")
}
