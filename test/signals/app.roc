app [main!] { pf: platform "./platform/main.roc" }

import pf.Elem

main! : {} => Elem
main! = |{}| {
    Elem.div([
        Elem.text("hello"),
        Elem.text("world"),
    ])
}
