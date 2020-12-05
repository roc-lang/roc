app "effect-example"
    packages { base: "./thing/platform-dir" }
    imports [ base.Task.{ Task, after } ]
    provides [ main ] to base

main : Task {}
main =
    when if 1 == 1 then True 3 else False 3.14 is
        True n -> Effect.putLine (Str.fromInt n)
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
