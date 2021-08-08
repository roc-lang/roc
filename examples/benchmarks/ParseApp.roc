app "parseap"
    packages { base: "platform" }
    imports [base.Task, MiniParsec]
    provides [ main ] to base

main : Task.Task {} []
main =
    MiniParsec.testResult "a" "xyz"
    |> Task.putLine

