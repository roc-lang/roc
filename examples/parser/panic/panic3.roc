app "panic"
    packages { base: "platform" }
    imports [base.Task]
    provides [ main ] to base

main : Task.Task {} []
main =

    q1 = "a"
    q2 = "b" 
    foo = [q1, q2, 3]


    List.len foo |>  Str.fromInt |>
       Task.putLine 

