interface Res
    exposes [ Res, withDefault, map, andThen, ConsList ]
    imports []

Res e a : [ Ok a, Err e ]

ConsList a : [ Cons a (ConsList a), Nil ]

# TODO FIXME for some reason, exposing this causes a stack overflow
# listMap : ConsList a, (a -> b) -> ConsList b
# listMap = \list, f ->
    # when list is
        # Nil -> Nil
        # Cons x xs -> Cons (f x) (listMap xs f)

map : Res e a, (a -> b) -> Res e b
map = \result, f ->
    when result is
        Ok v -> Ok (f v)
        Err e -> Err e

withDefault : Res x a, a -> a
withDefault = \result, default ->
    when result is
        Ok v -> v
        Err _ -> default

andThen : Res e a, (a -> Res e b) -> Res e b
andThen = \result, f ->
    when result is
        Ok v -> f v
        Err e -> Err e
