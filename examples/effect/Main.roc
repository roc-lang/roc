app Main provides [ main ] imports [ Effect ]

main : Effect.Effect {} as Fx
main =
    Effect.putLine "Write a thing!"
        |> Effect.after (\{} -> Effect.getLine)
        |> Effect.after (\line -> Effect.putLine line)
