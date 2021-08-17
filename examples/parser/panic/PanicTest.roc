app "test"
    packages { base: "platform" }
    imports [base.Task, Panic]
    provides [ main ] to base

main : Task.Task {} []
main =

    Str.fromInt Panic.foobar |>
       Task.putLine 

