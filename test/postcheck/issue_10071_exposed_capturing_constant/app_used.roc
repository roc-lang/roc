app [main] { pf: platform "./platform/main.roc" }

import pf.Browser
import pf.Elem exposing [Elem]

current = Browser.location

main : {} -> Elem
main = |_| Elem.Text("hello")
