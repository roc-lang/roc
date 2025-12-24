app [main] { pf: platform "./platform/main.roc" }

import pf.Simple

Model : { value: I64 }

main = {
    init: |{}| { value: 0 },
    update: |m, delta| { value: m.value + delta },
    render: |_m| Simple.leaf("hello"),
}
