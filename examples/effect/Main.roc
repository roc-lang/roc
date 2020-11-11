app Main provides [ main ] imports [ Effect, RBTree ]

foo : RBTree.Dict Int Int
foo = Empty # RBTree.empty

main : Effect.Effect {} as Fx
main =
    # if RBTree.isEmpty empty then
    if RBTree.size foo == 0 then
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
