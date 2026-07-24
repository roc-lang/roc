app [main] { pf: platform "./platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Http

main : {} -> Elem
main = |_| {
    task = Http.get_text_task("feed")
    _ = task
    Elem.Text("hello")
}
