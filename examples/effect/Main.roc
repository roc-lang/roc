app Main provides [ main ] imports [ Effect ]

main : Effect.Effect {} as Fx
main =
    Effect.putLine "Hello"
        |> Effect.after \{} -> Effect.putChar 87
        # |> Effect.after \{} -> Effect.putLine "orld"

