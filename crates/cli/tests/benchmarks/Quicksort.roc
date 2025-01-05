module [sort_by, sort_with, show]

show : List I64 -> Str
show = \list ->
    if List.isEmpty(list) then
        "[]"
    else
        content =
            list
            |> List.map(Num.toStr)
            |> Str.joinWith(", ")

        "[$(content)]"

sort_by : List a, (a -> Num *) -> List a
sort_by = \list, to_comparable ->
    sort_with(list, \x, y -> Num.compare(to_comparable(x), to_comparable(y)))

Order a : a, a -> [LT, GT, EQ]

sort_with : List a, (a, a -> [LT, GT, EQ]) -> List a
sort_with = \list, order ->
    n = List.len(list)

    quicksort_help(list, order, 0, (n - 1))

quicksort_help : List a, Order a, U64, U64 -> List a
quicksort_help = \list, order, low, high ->
    if low < high then
        when partition(low, high, list, order) is
            Pair(partition_index, partitioned) ->
                partitioned
                |> quicksort_help(order, low, Num.subSaturated(partition_index, 1))
                |> quicksort_help(order, (partition_index + 1), high)
    else
        list

partition : U64, U64, List a, Order a -> [Pair U64 (List a)]
partition = \low, high, initial_list, order ->
    when List.get(initial_list, high) is
        Ok(pivot) ->
            when partition_help(low, low, initial_list, order, high, pivot) is
                Pair(new_i, new_list) ->
                    Pair(new_i, swap(new_i, high, new_list))

        Err(_) ->
            Pair(low, initial_list)

partition_help : U64, U64, List c, Order c, U64, c -> [Pair U64 (List c)]
partition_help = \i, j, list, order, high, pivot ->
    if j < high then
        when List.get(list, j) is
            Ok(value) ->
                when order(value, pivot) is
                    LT | EQ ->
                        partition_help((i + 1), (j + 1), swap(i, j, list), order, high, pivot)

                    GT ->
                        partition_help(i, (j + 1), list, order, high, pivot)

            Err(_) ->
                Pair(i, list)
    else
        Pair(i, list)

swap : U64, U64, List a -> List a
swap = \i, j, list ->
    when Pair(List.get(list, i), List.get(list, j)) is
        Pair(Ok(at_i), Ok(at_j)) ->
            list
            |> List.set(i, at_j)
            |> List.set(j, at_i)

        _ ->
            []
