app "parseap"
    packages { base: "platform" }
    imports [base.Task, MiniParsec]
    provides [ main ] to base

main : Task.Task {} []
main =
    Task.after Task.getInt \n ->
        MiniParsec.idI64 n
            |> Str.fromInt
            |> Task.putLine
