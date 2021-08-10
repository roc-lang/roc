app "parseapp"
    packages { base: "platform" }
    imports [base.Task, Parser, Pair]
    provides [ main ] to base

main : Task.Task {} []
main =
    Parser.runToString Parser.showU8 "abcd" Parser.any
       |> Task.putLine

