app "effect-example" imports [ Effect, RBTree ] provides [ main ] to "./platform"

main : Effect.Effect {} as Fx
main =
    if RBTree.isEmpty (RBTree.insert 1 2 Empty) then
        Effect.putLine "Yay"
            |> Effect.after (\{} -> Effect.getLine)
            |> Effect.after (\line -> Effect.putLine line)
    else
        Effect.putLine "Nay"

