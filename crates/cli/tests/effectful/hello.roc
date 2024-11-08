app [main!] { pf: platform "../../../../examples/cli/effects-platform/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    Effect.putLine! "I'm an effect 👻"
