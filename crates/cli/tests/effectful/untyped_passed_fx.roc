app [main!] { pf: platform "../../../../examples/cli/effects-platform/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    logged! "hello" (\{} -> Effect.putLine! "Hello, World!")

logged! = \name, fx! ->
    Effect.putLine! "Before $(name)"
    fx! {}
    Effect.putLine! "After $(name)"
