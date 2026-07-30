# Companion for ./Main.roc, preserving the shape of the original report for
# issue #10156. Splits a list that begins with `delimiter`.
Split := [].{
    split_on : List(a), List(a) -> Try({ before : List(a), after : List(a) }, [NotFound]) where [a.is_eq : a, a -> Bool]
    split_on = |list, delimiter|
        if List.sublist(list, { start: 0, len: delimiter.len() }) == delimiter {
            Ok({ before: [], after: list.drop_first(delimiter.len()) })
        } else {
            Err(NotFound)
        }
}
