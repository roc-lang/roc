app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list : List(U64)
    list = []
    x = List.first(list)
    match x {
        Ok(_v) => Stdout.line!("got value")
        Err(ListWasEmpty) => Stdout.line!("was empty")
    }
}
