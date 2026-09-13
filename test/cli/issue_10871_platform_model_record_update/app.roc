app [Model, main] { pf: platform "./platform/main.roc" }

import pf.V

Model : { a : U64, b : U64 }

myrow = V.click(|m| m)

main = {
    init: |{}| { a: 0, b: 0 },
    view: |_m| myrow.append(V.click(|m| { ..m, a: 1 })),
}
