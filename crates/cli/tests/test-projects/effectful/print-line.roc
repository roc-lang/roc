app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    ["Welcome!", "What's your name?"]
    |> List.for_each!(Effect.put_line!)

    line = Effect.get_line!({})

    if line == "secret" then
        Effect.put_line!("You found the secret")
        Effect.put_line!("Congratulations!")
    else
        {}

    Effect.put_line!("You entered: $(line)")
    Effect.put_line!("It is known")
