interface ConsList exposes [ ConsList, empty, isEmpty, map, len ] imports []

ConsList a : [ Cons a (ConsList a), Nil ]

empty : ConsList a
empty = Nil

len : ConsList a -> Int *
len = \list ->
    when list is
        Cons _ rest ->
            1 + len rest

        Nil ->
            0

map : ConsList a, (a -> b) -> ConsList b
map = \list, f ->
    when list is
        Cons x xs ->
            Cons (f x) (map xs f)

        Nil ->
            Nil

isEmpty : ConsList a -> Bool
isEmpty = \list ->
    when list is
        Cons _ _ ->
            False

        Nil ->
            True
