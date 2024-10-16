app [main!] { pf: platform "effects-platform/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    line = Effect.getLine! {}

    if line == "secret" then
        Effect.putLine! "You found the secret"
        Effect.putLine! "Congratulations!"
    else
        {}

    Effect.putLine! "You entered: $(line)"
    Effect.putLine! "It is known"
