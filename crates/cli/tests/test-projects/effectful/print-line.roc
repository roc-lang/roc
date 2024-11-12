app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    ["Welcome!", "What's your name?"]
    |> forEach! Effect.putLine!

    line = Effect.getLine! {}

    if line == "secret" then
        Effect.putLine! "You found the secret"
        Effect.putLine! "Congratulations!"
    else
        {}

    Effect.putLine! "You entered: $(line)"
    Effect.putLine! "It is known"

forEach! : List a, (a => {}) => {}
forEach! = \l, f! ->
    when l is
        [] -> {}
        [x, .. as xs] ->
            f! x
            forEach! xs f!
