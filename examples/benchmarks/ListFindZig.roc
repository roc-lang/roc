app "list_find_zig"
    packages { pf: "platform/main.roc" }
    imports [pf.Task]
    provides [main] to pf

main : Task.Task {} []
main =
    Task.after
        Task.getInt
        \needle ->
            haystack = List.range 0 (needle + 1)
            when List.find haystack (\x -> x == needle) is
                Ok val ->
                    val
                        |> Num.toStr
                        |> Task.putLine
                _ ->
                    Task.succeed {}
