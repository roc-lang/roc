app [main!] { pf: platform "./platform/main.roc" }

import pf.Effect

main! = || {
    _ = Effect.things!()
    {}
}
