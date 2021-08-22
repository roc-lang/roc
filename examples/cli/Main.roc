app "cli-example"
    packages { base: "platform" }
    imports [ fx.Effect ]
    provides [ main ] to base

main : Effect.Effect {}
main =
    Effect.after Effect.getLine \lineThisThing -> Effect.putLine lineThisThing
