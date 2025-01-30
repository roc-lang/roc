app [quicksort] { pf: platform "quicksort-platform/main.roc" }

quicksort = \original_list ->
    n = List.len(original_list)

    quicksort_help(original_list, 0, (n - 1))

quicksort_help : List (Num a), U64, U64 -> List (Num a)
quicksort_help = \list, low, high ->
    if low < high then
        when partition(low, high, list) is
            Pair(partition_index, partitioned) ->
                partitioned
                |> quicksort_help(low, (partition_index - 1))
                |> quicksort_help((partition_index + 1), high)
    else
        list

partition : U64, U64, List (Num a) -> [Pair U64 (List (Num a))]
partition = \low, high, initial_list ->
    when List.get(initial_list, high) is
        Ok(pivot) ->
            when partition_help(low, low, initial_list, high, pivot) is
                Pair(new_i, new_list) ->
                    Pair(new_i, swap(new_i, high, new_list))

        Err(_) ->
            Pair(low, initial_list)

partition_help : U64, U64, List (Num c), U64, Num c -> [Pair U64 (List (Num c))]
partition_help = \i, j, list, high, pivot ->
    if j < high then
        when List.get(list, j) is
            Ok(value) ->
                if value <= pivot then
                    partition_help((i + 1), (j + 1), swap(i, j, list), high, pivot)
                else
                    partition_help(i, (j + 1), list, high, pivot)

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
            # to prevent a decrement on list
            # turns out this is very important for optimizations
            list
