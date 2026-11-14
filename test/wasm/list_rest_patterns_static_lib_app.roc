app [main!] { pf: platform "./static-lib-platform/main.roc" }

# List rest patterns over lists of refcounted items. A rest from the front of
# a uniquely owned list releases the items behind it right away, so `inner`,
# dropped that way, is uniquely owned again and grows in place. Rests that
# skip leading items, and rests of a list that stays live, give the right
# items. The program allocates only `inner`, `outer`, `shared` and its first
# item.
drop_back : List(List(U8)) -> List(List(U8))
drop_back = |items| match items {
    [.. as rest, _] => rest
    [] => []
}

drop_front : List(List(U8)) -> List(List(U8))
drop_front = |items| match items {
    [_, .. as rest] => rest
    [] => []
}

middle : List(List(U8)) -> List(List(U8))
middle = |items| match items {
    [_, .. as rest, _] => rest
    _ => []
}

main! = |seed| {
    inner = List.with_capacity(8).append(seed.to_u8_wrap())
    outer = [[1.U8], [2, 2], inner]
    rest = drop_back(outer)
    grown = inner.append(9)
    released = !rest.contains(grown) and rest.len() == 2

    shared = [[seed.to_u8_wrap() + 1], [3, 3, 3], [4, 4, 4, 4], [5]]
    front = drop_front(shared)
    inside = middle(shared)
    back = drop_back(shared)
    views = front.len() == 3 and inside.len() == 2 and back.len() == 3

    shared_sizes = (shared.len(), front.first() ?? [], back.last() ?? [])
    items_ok = shared_sizes.0 == 4 and shared_sizes.1 == [3, 3, 3] and shared_sizes.2 == [4, 4, 4, 4]
    inside_ok = inside == [[3, 3, 3], [4, 4, 4, 4]]

    if released and views and items_ok and inside_ok and grown.len() == 2 "ok" else "bad"
}
