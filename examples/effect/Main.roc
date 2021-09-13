app "effect-example"
    packages { base: "thing/platform-dir" }
    imports [fx.Effect]
    provides [ main ] to base

main : Effect.Effect {}
main =
    Effect.after (Effect.getLine) \line ->
        Effect.after (Effect.putLine "You entered: \(line)") \{} ->
            Effect.after (Effect.putLine "It is known") \{} ->
                Effect.always {}
