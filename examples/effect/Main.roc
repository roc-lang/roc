app Main provides [ main ] imports [ Effect, ConsList ]

empty : ConsList.ConsList Int
empty = ConsList.empty

main : Effect.Effect {} as Fx
main =
    # if ConsList.isEmpty empty then
    if ConsList.len empty == 0 then
        Effect.putLine "Yay"
            |> Effect.after (\{} -> Effect.getLine)
            |> Effect.after (\line -> Effect.putLine line)
    else
        Effect.putLine "Nay"


#    Effect.always "Write a thing"
#        |> Effect.map (\line -> Str.concat line "!")
#        |> Effect.after (\line -> Effect.putLine line)
#        |> Effect.after (\{} -> Effect.getLine)
#        |> Effect.after (\line -> Effect.putLine line)
