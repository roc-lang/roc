module [swap, partition, quicksort]

quicksort : List (Num a), U64, U64 -> List (Num a)
quicksort = \list, low, high ->
    when partition(low, high, list) is
        Pair(partition_index, partitioned) ->
            partitioned
            |> quicksort(low, (partition_index - 1))
            |> quicksort((partition_index + 1), high)

swap : U64, U64, List a -> List a
swap = \i, j, list ->
    when Pair(List.get(list, i), List.get(list, j)) is
        Pair(Ok(at_i), Ok(at_j)) ->
            list
            |> List.set(i, at_j)
            |> List.set(j, at_i)

        _ ->
            []

partition : U64, U64, List (Num a) -> [Pair U64 (List (Num a))]
partition = \low, high, initial_list ->
    when List.get(initial_list, high) is
        Ok(pivot) ->
            when partition_help((low - 1), low, initial_list, high, pivot) is
                Pair(new_i, new_list) ->
                    Pair((new_i + 1), swap((new_i + 1), high, new_list))

        Err(_) ->
            Pair((low - 1), initial_list)

partition_help : U64, U64, List (Num a), U64, Num a -> [Pair U64 (List (Num a))]
partition_help = \i, j, list, high, pivot ->
    if j < high then
        when List.get(list, j) is
            Ok(value) ->
                if value <= pivot then
                    partition_help((i + 1), (j + 1), swap((i + 1), j, list), high, pivot)
                else
                    partition_help(i, (j + 1), list, high, pivot)

            Err(_) ->
                Pair(i, list)
    else
        Pair(i, list)
