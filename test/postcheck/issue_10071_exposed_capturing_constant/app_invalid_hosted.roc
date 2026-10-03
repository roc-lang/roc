app [main] { pf: platform "./platform/invalid_hosted.roc" }

import pf.Elem

main : {} -> Elem
main = |_| Elem.Text("hello")
