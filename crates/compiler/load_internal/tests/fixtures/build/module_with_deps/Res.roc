module [Res, with_default, map, list_map, and_then, ConsList]

Res ok err : [Ok ok, Err err]

ConsList a : [Cons a (ConsList a), Nil]

list_map : ConsList a, (a -> b) -> ConsList b
list_map = \list, f ->
    when list is
        Nil -> Nil
        Cons(x, xs) -> Cons(f(x), list_map(xs, f))

map : Res a err, (a -> b) -> Res b err
map = \result, transform ->
    when result is
        Ok(ok) -> Ok(transform(ok))
        Err(err) -> Err(err)

with_default : Res a err, a -> a
with_default = \result, default ->
    when result is
        Ok(ok) -> ok
        Err(_) -> default

and_then : Res a err, (a -> Res b err) -> Res b err
and_then = \result, transform ->
    when result is
        Ok(ok) -> transform(ok)
        Err(err) -> Err(err)
