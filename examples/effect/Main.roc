app "effect-example"
    packages { base: "thing/platform-dir" }
    imports [fx.Effect]
    provides [ main ] to base

main : Effect.Effect {}
main =
    Effect.after (Effect.putLine "Enter some input:") \{} ->
        Effect.after Effect.getLine \lineThisThing ->
            Effect.after (Effect.putLine "You entered:") \{} ->
                Effect.after (Effect.putLine lineThisThing) \{} ->
                    Effect.always {}
