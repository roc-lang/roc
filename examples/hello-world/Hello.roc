app "hello-world"
    packages { base: "platform" }
    imports []
    provides [ main ] to base


ConsList a : [ Cons a (ConsList a), Nil ]

empty : ConsList a
empty = Nil

isEmpty : ConsList a -> Bool
isEmpty = \list ->
    when list is
        Cons _ _ ->
            False

        Nil ->
            True


greeting =
    hi = "Hello"
    name = "World"

    "\(hi), \(name)!!!!!!!!!!!!!"

main = 
    myList : ConsList (Int *)
    myList = empty


    if isEmpty myList then greeting else greeting
