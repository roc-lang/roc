app "parseap"
    packages { base: "platform" }
    imports [base.Task, MiniParsec]
    provides [ main ] to base

main : Task.Task {} []
main =
    Task.after Task.getInt \n ->
        MiniParsec.test1 n
            |> Task.putLine
