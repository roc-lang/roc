app Main provides [ main ] imports [ Effect ]

main : Effect.Effect {} as Fx
main =
    Effect.putLine "Write a thing!"
        |> Effect.after (\{} -> Effect.getLine 3)
        |> Effect.after (\line -> Effect.putLine line)

