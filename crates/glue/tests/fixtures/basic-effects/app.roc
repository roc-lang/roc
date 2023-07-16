app "app"
    packages { pf: "platform.roc" }
    imports [pf.Effect]
    provides [main] to pf

main =
    {} <- Effect.putLine "Enter some text:" |> Effect.after
    text <- Effect.getLine |> Effect.after

    Effect.putLine "The text was: \(text)"
