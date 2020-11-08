app Main provides [ main ] imports [ Effect ]

main : Effect.Effect {} as Fx
main =
    Effect.putLine "Write a thing!"
        |> Effect.after (\{} -> Effect.getLine 1)
        |> Effect.after (\line -> Effect.putLine line)

