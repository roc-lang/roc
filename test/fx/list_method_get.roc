app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    my_list = [8]
    foo = my_list.get(0)
    if Try.is_ok(foo) {
        Stdout.line!("is ok")
    } else {
        Stdout.line!("is err")
    }
}
