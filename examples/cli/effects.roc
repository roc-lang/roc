app "effects"
    packages { pf: "effects-platform/main.roc" }
    imports [pf.Effect]
    provides [main] to pf

main : Effect.Effect {}
main =
    Effect.after
        (Effect.getLine)
        \line ->
            Effect.after
                (Effect.putLine "You entered: \(line)")
                \{} ->
                    Effect.after
                        (Effect.putLine "It is known")
                        \{} ->
                            Effect.always {}
