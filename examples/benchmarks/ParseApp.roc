app "parseapp"
    packages { base: "platform" }
    imports [base.Task, MiniParsec, Pair]
    provides [ main ] to base

main : Task.Task {} []
main =
    Pair.testSecond 0 0
       |> Task.putLine

