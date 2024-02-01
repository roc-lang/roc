app "effects"
    packages { pf: "effects-platform/main.roc" }
    provides [main] to pf

import pf.Effect

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
