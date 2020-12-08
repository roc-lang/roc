app "effect-example"
    packages { base: "thing/platform-dir" }
    imports [ base.Task.{ Task, after } ]
    provides [ main ] to base

# TODO `main : Task {}` does not work
# it will then think that the `Task` module is unused
# (if we also don't use any of the other importd symbols)
main : Task.Task {}
main =
    when if 1 == 1 then True 3 else False 3.14 is
        True n -> Task.putLine (Str.fromInt n)
        _ -> Task.putLine "Yay"

# main : Effect.Effect {} as Fx
# main =
#     if RBTree.isEmpty (RBTree.insert 1 2 Empty) then
#         Effect.putLine "Yay"
#             |> Effect.after (\{} -> Effect.getLine)
#             |> Effect.after (\line -> Effect.putLine line)
#     else
#         Effect.putLine "Nay"
# 
