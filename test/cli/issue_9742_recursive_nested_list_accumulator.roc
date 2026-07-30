package [] {}

# Repro for https://github.com/roc-lang/roc/issues/9742.

keep : List(a), (a -> Bool) -> List(a)
keep = |list, predicate| {
    loop = |sub_list, kept_items| {
        match sub_list {
            [] => kept_items
            [first, .. as rest] => {
                if predicate(first) {
                    rest->loop(kept_items.append(first))
                } else {
                    rest->loop(kept_items)
                }
            }
        }
    }

    loop(list, [])
}

expect {
    list = [[1, 2, 3], [5, 5, 5], [5, 1, 2], [2, 1, 2], [1, 5, 2], [2, 2, 1], [1, 2, 5]]
    result = list->keep(|x| x.contains(5))
    expected = [[5, 5, 5], [5, 1, 2], [1, 5, 2], [1, 2, 5]]
    result == expected
}
