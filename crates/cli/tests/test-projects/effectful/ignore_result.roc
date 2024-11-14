app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    _ = Effect.getLine! {}
    Effect.putLine! "I asked for input and I ignored it. Deal with it! 😎"
