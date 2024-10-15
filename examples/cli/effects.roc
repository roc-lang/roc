app [main] { pf: platform "effects-platform/main.roc" }

import pf.Effect

main : {} => {}
main = \{} ->
    line = Effect.getLine! {}
    Effect.putLine! "You entered: $(line)"
    Effect.putLine! "It is known"
