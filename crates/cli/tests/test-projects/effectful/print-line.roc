app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    ["Welcome!", "What's your name?"]
    |> List.forEach! Effect.putLine!

    line = Effect.getLine! {}

    if line == "secret" then
        Effect.putLine! "You found the secret"
        Effect.putLine! "Congratulations!"
    else
        {}

    Effect.putLine! "You entered: $(line)"
    Effect.putLine! "It is known"
