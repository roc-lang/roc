app "test"
    packages { base: "platform" }
    imports [base.Task]
    provides [ main ] to base

main : Task.Task {} []
main =

    foo = {a: 1 b: 2}


    foo.a + foo.b |>  Str.fromInt |>
       Task.putLine 

