app [main!] { pf: platform "./platform/main.roc" }

import pf.Elem

main! : {} => Elem
main! = |{}| {
    Elem.text("hello")
}
