app Main provides [ main ] imports [ Effect ]

main : Effect.Effect {} as Fx
main =
    Effect.always "Write a thing"
        |> Effect.map (\line -> Str.concat line "!")
        |> Effect.after (\line -> Effect.putLine line)
        |> Effect.after (\{} -> Effect.getLine)
        |> Effect.after (\line -> Effect.putLine line)
