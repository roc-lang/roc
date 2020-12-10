app "effect-example"
    packages { base: "thing/platform-dir" }
    imports [ base.Task ]
    provides [ main ] to base

# main : Task.Task {} F64

main =
    Task.after (Task.putLine "foo") \{} -> Task.putLine "bar"



# main =
#     # Task.after (Task.putLine "foo") \{} -> Task.putLine "bar"
#     y = Task.ealways 3.14
# 
#     a = y |> Task.emap (\x -> x)
#     b = y |> Task.emap (\x -> x)
# 
#     if 1 == 1 then a else b
