app [main] { pf: platform "./platform/main.roc" }

import pf.Browser
import pf.Elem

current = Browser.location.map(|_| "ok")

main : {} -> Elem
main = |_| Elem.Text("hello")
