GenericFold := [].{
    append_all : List(a), List(a) -> List(a)
    append_all = |initial, items| items.fold(initial, |acc, item| acc.append(item))

    forward : (List(a), a -> List(a)), List(a), a -> List(a)
    forward = |next, acc, item| next(acc, item)

    append_deferred : List(a), List(a) -> List(a)
    append_deferred = |initial, items| items.fold(initial, |acc, item| forward(|xs, x| xs.append(x), acc, item))

    invoke_three : (List(a), a, List(a) -> List(a)), List(a), a, List(a) -> List(a)
    invoke_three = |callback, acc, item, unused| callback(acc, item, unused)

    discard_field : List(a), a, List(a) -> List(a)
    discard_field = |acc, item, discarded| invoke_three(|xs, x, _| xs.append(x), acc, item, discarded)

    duplicate_fields : List(a), a -> (List(a), List(a))
    duplicate_fields = |initial, item| {
        use_both = |left, right, x| (left.append(x), right)
        use_both(initial, initial, item)
    }
}
