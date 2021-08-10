app "JimTest"
    packages { base: "platform" }
    imports [base.Task, Loop]
    provides [ main ] to base

main : Task.Task {} []
main =
    # Loop.test1
    # Loop.test2
    Loop.test3
       |> Task.putLine

