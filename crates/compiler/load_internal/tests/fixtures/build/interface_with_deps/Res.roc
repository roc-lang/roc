interface Res
    exposes [Res, withDefault, map, listMap, andThen, ConsList]

Res ok err : [Ok ok, Err err]

ConsList a : [Cons a (ConsList a), Nil]

listMap : ConsList a, (a -> b) -> ConsList b
listMap = \list, f ->
    when list is
        Nil -> Nil
        Cons x xs -> Cons (f x) (listMap xs f)

map : Res a err, (a -> b) -> Res b err
map = \result, transform ->
    when result is
        Ok ok -> Ok (transform ok)
        Err err -> Err err

withDefault : Res a err, a -> a
withDefault = \result, default ->
    when result is
        Ok ok -> ok
        Err _ -> default

andThen : Res a err, (a -> Res b err) -> Res b err
andThen = \result, transform ->
    when result is
        Ok ok -> transform ok
        Err err -> Err err
