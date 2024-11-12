app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    logged! "hello" (\{} -> Effect.putLine! "Hello, World!")

logged! = \name, fx! ->
    Effect.putLine! "Before $(name)"
    fx! {}
    Effect.putLine! "After $(name)"
