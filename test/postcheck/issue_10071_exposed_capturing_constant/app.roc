app [main] { pf: platform "./platform/main.roc" }

import pf.Elem exposing [Elem]

main : {} -> Elem
main = |_| Elem.Text("hello")
