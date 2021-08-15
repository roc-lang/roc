app "effect-example"
    packages { base: "thing/platform-dir" }
    imports [fx.Effect]
    provides [ main ] to base

main : Effect.Effect {}
main =
    Effect.after Effect.getLine \lineThisThing -> Effect.putLine lineThisThing
