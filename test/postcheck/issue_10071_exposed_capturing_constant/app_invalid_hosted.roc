app [main] { pf: platform "./platform/invalid_hosted.roc" }

import pf.Elem exposing [Elem]

main : {} -> Elem
main = |_| Elem.Text("hello")
