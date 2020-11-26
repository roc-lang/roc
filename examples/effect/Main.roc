app "effect-example" imports [ Effect ] provides [ main ] to "./platform"


main : Effect.Effect {} as Fx
main =
    when if 1 == 1 then True 3 else False 3.14 is
        True 3 -> Effect.putLine "Yay"
        _ -> Effect.putLine "Yay"

# main : Effect.Effect {} as Fx
# main =
#     if RBTree.isEmpty (RBTree.insert 1 2 Empty) then
#         Effect.putLine "Yay"
#             |> Effect.after (\{} -> Effect.getLine)
#             |> Effect.after (\line -> Effect.putLine line)
#     else
#         Effect.putLine "Nay"
# 
