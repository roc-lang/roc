app "parseapp"
    packages { base: "platform" }
    imports [base.Task, MiniParsec]
    provides [ main ] to base

main : Task.Task {} []
main =
    MiniParsec.testResult2 "UUU" "xyz"
       |> Task.putLine

