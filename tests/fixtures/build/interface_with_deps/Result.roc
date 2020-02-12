interface Result
    exposes [ Result, withDefault, map, andThen, ConsList ]
    imports []

Result e a : [ Ok a, Err e ]

ConsList a : [ Cons a (ConsList a), Nil ]

listMap : ConsList a, (a -> b) -> ConsList b
listMap = \list, f ->
    when list is
        Nil -> Nil
        Cons x xs -> Cons (f x) (listMap xs f)

map : Result e a, (a -> b) -> Result e b
map = \result, f ->
    when result is
        Ok v -> Ok (f v)
        Err e -> Err e

withDefault : Result x a, a -> a
withDefault = \result, default ->
    when result is
        Ok v -> v
        Err _ -> default

andThen : Result e a, (a -> Result e b) -> Result e b
andThen = \result, f ->
    when result is
        Ok v -> f v
        Err e -> Err e
