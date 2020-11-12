app Main provides [ main ] imports [ Effect, RBTree ]

toAndFro : Int
toAndFro =
    empty : RBTree.Dict Int Int
    empty = RBTree.empty

    empty
        |> RBTree.toList
        |> List.len




main : Effect.Effect {} as Fx
main =
    # if RBTree.isEmpty empty then
    if toAndFro == 2 then
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
